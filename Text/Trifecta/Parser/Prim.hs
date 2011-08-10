{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleContexts #-}
module Text.Trifecta.Parser.Prim 
  ( Parser(..)
  , why
  , stepParser
  ) where

import Data.Set as Set
import Data.ByteString
import Data.Bifunctor
import Text.PrettyPrint.Free
import Text.Trifecta.It
import Text.Trifecta.Delta
import Text.Trifecta.Diagnostic
import Text.Trifecta.Parser.Err
import Text.Trifecta.Parser.Err.State
import Text.Trifecta.Parser.Step

data Parser e a = Parser 
  { unparser :: forall r.
    (a -> ErrState e -> Delta -> ByteString -> It r) ->  -- uncommitted ok
    (ErrState e -> Delta -> ByteString -> Ir r) ->       -- uncommitted err
    (a ->  ErrState e -> Delta -> ByteString -> It r) -> -- committed ok
    (ErrState e -> Delta -> ByteString -> It r) ->       -- committed err
    ErrState e -> Delta -> ByteString -> It r
  } 

instance Functor (Parser e) where
  fmap f (Parser m) = Parser $ \ eo ee co -> m (eo . f) ee (co . f)
  {-# INLINE fmap #-}

instance Applicative (Parser e) where
  pure a = Parser $ \ eo _ _ _ -> eo a 
  {-# INLINE pure #-}
  Parser m <*> Parser n = Parser $ \ eo ee co ce -> 
    m (\f -> n (eo . f) ee (co . f) ce) ee
      (\f -> n (co . f) ce (co . f) ce) ce
  {-# INLINE (<*>) #-}

instance Alternative (Parser e) where
  empty = Parser $ \eo ee co ce -> ee
  {-# INLINE empty #-}
  Parser m <|> Parser n = Parser $ \ eo ee co ce -> m eo (n eo ee co ce) co ce
  {-# INLINE (<|>) #-}

instance Monad (Parser e) where
  return a = Parser $ \ eo _ _ _ -> eo a 
  {-# INLINE return #-}
  Parser m >>= k = Parser $ \ eo ee co ce -> 
    m (\a -> unparser (k a) eo ee co ce) ee 
      (\a -> unparser (k a) co ce co ce) ce
  {-# INLINE (>>=) #-}
  fail = throwError . FailErr
  {-# INLINE fail #-}

instance MonadPlus (Parser e) where
  mzero = mempty
  mplus = (<|>) 

instance MonadWriter (Seq (Diagnostic e)) (Parser e) where
  tell w = Parser $ \eo _ _ _ e -> eo () e { errLog = errLog e <> w }
  {-# INLINE tell #-}
  listen (Parser m) = Parser $ \eo ee co ce -> 
    m (\ a e -> eo (a,errLog e) e) ee 
      (\ a e -> co (a,errLog e) e) ce 
  pass (Parser m) = Parser $ \eo ee co ce -> 
    m (\(a,p) e -> eo a e { errLog = p $ errLog e }) ee
      (\(a,p) e -> co a e { errLog = p $ errLog e }) ce

instance MonadError (Err e) (Parser e) where
  throwError m = Parser $ \_ ee _ _ _ e -> ee e { errMessage = errMessage e <> m } 
  {-# INLINE throwError #-}
  catchError (Parser p) k = Parser $ \ eo ee co ce e -> p eo (\e' -> unparser (k e') eo ee co ce e) co ce e
  {-# INLINE catchError #-}

instance MonadParser e (Parser e) where
  commit (Parser m) = Parser $ \ _ _ co ce -> m co ce co ce
  {-# INLINE commit #-}
  labels (Parser p) msgs = Parser $ \ eo ee -> p
     (\a e -> eo a (if errMessage e == EmptyErr then e else e { errExpected = errExpected e `union` msgs }))
     (\e -> ee e { errExpected = errExpected e `union` msgs })
  {-# INLINE labels #-}
  it m = Parser $ \ eo _ _ _ e d bs -> do 
     a <- m
     eo a e d bs
  {-# INLINE it #-}
  mark = Parser $ \eo ee _ _ s e d -> eo d s e d
  {-# INLINE mark #-}
  release d = Parser $ \_ eo _ _ _ s e _ _ w -> do
    dbs <- refill d
    case dbs of
      JustPair d' bs -> eo e d' bs w
      NothingPair -> ee 
  {-# INLINE release #-}
  line = Parser $ \_ eo _ _ _ s e d bs -> eo bs s e d bs
  {-# INLINE line #-}
  satisfy f = Parser $ \ eo ee co ce e d bs = 
    case UTF8.uncons $ drop (columnByte d) bs of
      Nothing -> ee e { errMessage = EndOfInputErr } d bs
      Just (c, xs) 
        | not (f c) -> ee e d bs
        | Strict.null xs -> fillIt >>= \dbs -> case dbs of
          JustPair d' bs' -> eo c e d' bs'
          Nothing -> eo c e (d <> delta c) bs -- END OF LINE
        | otherwise -> eo c e (d <> delta c) bs 
  {-# INLINE satisfy #-}
  satisfyAscii f = Parser $ \ eo ee co ce e d bs = 
    let b = columnByte d in
    case fromEnum . toEnum <$> elemIndex b bs of
      Nothing -> ee e { errMessage = EndOfInputErr } d bs
      Just c 
        | not (f c) -> ee e d bs
        | b == length bs - 1 -> fillIt >>= \dbs -> case dbs of
          JustPair d' bs' -> eo c e d' bs'
          Nothing -> eo c e (d <> delta c) bs -- END OF LINE
        | otherwise -> eo c e (d <> delta c) bs
  {-# INLINE satisfyAscii #-}

data St e a = JuSt a !(ErrState e) !Delta !ByteString
            | NoSt !(ErrState e) !Delta !ByteString

instance Bifunctor St where
  bimap f g (JuSt a e d bs) = JuSt (g a) (fmap f e) d bs
  bimap f _ (NoSt e d bs) = NoSt (fmap f e) d bs

stepParser :: (ErrState e -> Delta -> ByteString -> Diagnostic e) ->
              Parser e a -> ErrState e -> Delta -> ByteString -> Step e a
stepParser y (Parser p) e0 d0 bs0 = 
  go mempty $ p ju no ju no e0 d0 bs0
  where
    ju a e d bs = Pure (JuSt a e d bs)
    no e d bs = Pure (NoSt e d bs)
    go r (Pure (JuSt a e _ _)) = StepDone r (errLog e) a
    go r (Pure (NoSt e d bs)) = StepFail r (errLog e) $ y e d bs
    go r (Cont ma k) = StepCont r (case ma of
                                     JuSt a e _ _ -> Success (errLog e) a
                                     NoSt e d bs  -> Failure (errLog e) (y e d bs)) 
                                  (go <*> k)

why :: ErrState e -> Delta -> ByteString -> Diagnostic (Doc e)
why (ErrState ss m _) d bs 
  | Set.null ss = diagnose (surface d bs) m
  | otherwise   = expected <$> diagnose (surface d bs) m 
  where
    expected d = d <> text ", expected" <+> fillSep (punctuate (char ',') (toList ss))

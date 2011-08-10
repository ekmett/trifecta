{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleContexts, Rank2Types, FlexibleInstances #-}
module Text.Trifecta.Parser.Prim 
  ( Parser(..)
  , why
  , stepParser
  , parseTest
  ) where

import Control.Applicative
import Control.Monad.Error.Class
import Control.Monad.Writer.Class
import Control.Monad
import Data.Functor.Plus
import Data.Semigroup
import Data.Foldable
import Data.Monoid
import Data.Functor.Bind
import Data.Set as Set hiding (empty, toList)
import Data.ByteString as Strict hiding (empty)
import Data.Sequence as Seq hiding (empty)
import Data.ByteString.UTF8 as UTF8
import Data.Bifunctor
import Text.PrettyPrint.Free
import Text.Trifecta.Delta
import Text.Trifecta.Diagnostic
import Text.Trifecta.Render.Prim
import Text.Trifecta.Parser.Class
import Text.Trifecta.Parser.It
import Text.Trifecta.Parser.Err
import Text.Trifecta.Parser.Err.State
import Text.Trifecta.Parser.Step
import Text.Trifecta.Parser.Result
import Text.Trifecta.Util.MaybePair
import System.Console.Terminfo.PrettyPrint

data Parser e a = Parser 
  { unparser :: forall r.
    (a -> ErrState e -> Delta -> ByteString -> It r) ->  -- uncommitted ok
    (ErrState e -> Delta -> ByteString -> It r) ->       -- uncommitted err
    (a ->  ErrState e -> Delta -> ByteString -> It r) -> -- committed ok
    (ErrState e -> Delta -> ByteString -> It r) ->       -- committed err
    ErrState e -> Delta -> ByteString -> It r
  } 

instance Functor (Parser e) where
  fmap f (Parser m) = Parser $ \ eo ee co -> m (eo . f) ee (co . f)
  {-# INLINE fmap #-}

instance Apply (Parser e) where (<.>) = (<*>)
instance Applicative (Parser e) where
  pure a = Parser $ \ eo _ _ _ -> eo a 
  {-# INLINE pure #-}
  Parser m <*> Parser n = Parser $ \ eo ee co ce -> 
    m (\f -> n (eo . f) ee (co . f) ce) ee
      (\f -> n (co . f) ce (co . f) ce) ce
  {-# INLINE (<*>) #-}

instance Alt (Parser e) where (<!>) = (<|>)
instance Plus (Parser e) where zero = empty
instance Alternative (Parser e) where
  empty = Parser $ \_ ee _ _ -> ee
  {-# INLINE empty #-}
  Parser m <|> Parser n = Parser $ \ eo ee co ce -> m eo (n eo ee co ce) co ce
  {-# INLINE (<|>) #-}

instance Bind (Parser e) where (>>-) = (>>=)
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
  mzero = empty
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
  throwError m = Parser $ \_ ee _ _ e -> ee e { errMessage = errMessage e <> m } 
  {-# INLINE throwError #-}
  catchError (Parser p) k = Parser $ \ eo ee co ce e -> p eo (\e' -> unparser (k (errMessage e')) eo ee co ce e) co ce e
  {-# INLINE catchError #-}

instance MonadParser (Parser e) where
  commit (Parser m) = Parser $ \ _ _ co ce -> m co ce co ce
  unexpected s = Parser $ \ _ ee _ _ e -> ee e { errMessage = UnexpectedErr s } 
  {-# INLINE commit #-}
  labels (Parser p) msgs = Parser $ \ eo ee -> p
     (\a e -> eo a (if knownErr (errMessage e) then e { errExpected = errExpected e `union` msgs } else e))
     (\e -> ee e { errExpected = errExpected e `union` msgs })
  {-# INLINE labels #-}
  it m = Parser $ \ eo _ _ _ e d bs -> do 
     a <- m
     eo a e d bs
  {-# INLINE it #-}
  mark = Parser $ \eo _ _ _ e d -> eo d e d
  {-# INLINE mark #-}
  release d' = Parser $ \eo ee _ _ e d bs -> do
    mbs <- lineIt d'
    case mbs of
      Just bs' -> eo () e d' bs'
      Nothing -> ee e d bs
  {-# INLINE release #-}
  line = Parser $ \eo _ _ _ e d bs -> eo bs e d bs
  {-# INLINE line #-}
  satisfy f = Parser $ \ eo ee _ _ e d bs ->
    case UTF8.uncons $ Strict.drop (columnByte d) bs of
      Nothing -> ee e { errMessage = EndOfFileErr } d bs
      Just (c, xs) 
        | not (f c) -> ee e d bs
        | Strict.null xs -> fillIt (d <> delta c) >>= \dbs -> case dbs of
          JustPair d' bs' -> eo c e d' bs'
          NothingPair -> eo c e (d <> delta c) bs -- END OF LINE
        | otherwise -> eo c e (d <> delta c) bs 
  {-# INLINE satisfy #-}
  satisfyAscii f = Parser $ \ eo ee _ _ e d bs ->
    let b = columnByte d in
    if b >= 0 && b < Strict.length bs 
    then case toEnum $ fromEnum $ Strict.index bs b of
      c | not (f c) -> ee e d bs
        | b == Strict.length bs - 1 -> fillIt (d <> delta c) >>= \dbs -> case dbs of
          JustPair d' bs' -> eo c e d' bs'
          NothingPair -> eo c e (d <> delta c) bs
        | otherwise -> eo c e (d <> delta c) bs
    else ee e { errMessage = EndOfFileErr } d bs
  {-# INLINE satisfyAscii #-}

data St e a = JuSt a !(ErrState e) !Delta !ByteString
            | NoSt !(ErrState e) !Delta !ByteString

instance Bifunctor St where
  bimap f g (JuSt a e d bs) = JuSt (g a) (fmap f e) d bs
  bimap f _ (NoSt e d bs) = NoSt (fmap f e) d bs

stepParser :: (Diagnostic e -> Diagnostic t) -> 
              (ErrState e -> Delta -> ByteString -> Diagnostic t) ->
              Parser e a -> ErrState e -> Delta -> ByteString -> Step t a
stepParser yl y (Parser p) e0 d0 bs0 = 
  go mempty $ p ju no ju no e0 d0 bs0
  where
    ju a e d bs = Pure (JuSt a e d bs)
    no e d bs = Pure (NoSt e d bs)
    go r (Pure (JuSt a e _ _)) = StepDone r (yl <$> errLog e) a
    go r (Pure (NoSt e d bs))  = StepFail r (yl <$> errLog e) $ y e d bs
    go r (It ma k) = StepCont r (case ma of
                                   JuSt a e _ _ -> Success (yl <$> errLog e) a
                                   NoSt e d bs  -> Failure (yl <$> errLog e) (y e d bs)) 
                                (go <*> k)

why :: (e -> Doc t) -> ErrState e -> Delta -> ByteString -> Diagnostic (Doc t)
why pp (ErrState ss m _) d bs 
  | Set.null ss = diagnose pp (surface d bs) m
  | otherwise   = expected <$> diagnose pp (surface d bs) m 
  where
    expected doc = doc <> text ", expected" <+> fillSep (punctuate (char ',') $ text <$> toList ss)

parseTest :: Show a => Parser TermDoc a -> ByteString -> IO ()
parseTest p bs = case eof (feed st bs) of
  Failure xs e -> displayLn $ prettyTerm $ toList (xs |> e)
  Success xs a -> do 
    displayLn $ prettyTerm $ toList xs
    print a
  where st = stepParser id (why id) (release mempty *> p) mempty mempty bs

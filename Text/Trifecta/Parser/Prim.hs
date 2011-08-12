{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleContexts, Rank2Types, FlexibleInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Trifecta.Parser.Prim
-- Copyright   :  (c) Edward Kmett 2011
-- License     :  BSD3
-- 
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable
-- 
-----------------------------------------------------------------------------
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
import Data.Functor.Bind (Apply(..), Bind((>>-)))
import Data.Set as Set hiding (empty, toList)
import Data.ByteString as Strict hiding (empty)
import Data.Sequence as Seq hiding (empty)
import Data.ByteString.UTF8 as UTF8
import Data.Bifunctor
import Text.PrettyPrint.Free hiding (line)
import Text.Trifecta.Rope.Delta
import Text.Trifecta.Rope.Prim
import Text.Trifecta.Diagnostic.Class
import Text.Trifecta.Diagnostic.Prim
import Text.Trifecta.Diagnostic.Level
import Text.Trifecta.Diagnostic.Err
import Text.Trifecta.Diagnostic.Err.State
import Text.Trifecta.Diagnostic.Rendering.Prim
import Text.Trifecta.Diagnostic.Rendering.Caret
import Text.Trifecta.Parser.Class
import Text.Trifecta.Parser.It
import Text.Trifecta.Parser.Step
import Text.Trifecta.Parser.Result
import System.Console.Terminfo.PrettyPrint

data Parser e a = Parser 
  { unparser :: forall r.
    (a -> ErrState e -> Delta -> ByteString -> It Rope r) ->  -- uncommitted ok
    (ErrState e -> Delta -> ByteString -> It Rope r) ->       -- uncommitted err
    (a ->  ErrState e -> Delta -> ByteString -> It Rope r) -> -- committed ok
    (ErrState e -> Delta -> ByteString -> It Rope r) ->       -- committed err
    Delta -> ByteString -> It Rope r
  } 

instance Functor (Parser e) where
  fmap f (Parser m) = Parser $ \ eo ee co -> m (eo . f) ee (co . f)
  {-# INLINE fmap #-}
  a <$ Parser m = Parser $ \ eo ee co -> m (\_ -> eo a) ee (\_ -> co a)
  {-# INLINE (<$) #-}

instance Apply (Parser e) where (<.>) = (<*>)
instance Applicative (Parser e) where
  pure a = Parser $ \ eo _ _ _ -> eo a mempty
  {-# INLINE pure #-}
  Parser m <*> Parser n = Parser $ \ eo ee co ce -> 
    m (\f e -> n (\a e' -> eo (f a) (e <> e')) ee (\a e' -> co (f a) (e <> e')) ce) ee
      (\f e -> n (\a e' -> co (f a) (e <> e')) ce (\a e' -> co (f a) (e <> e')) ce) ce
  {-# INLINE (<*>) #-}
  Parser m <* Parser n = Parser $ \ eo ee co ce -> 
    m (\a e -> n (\_ e' -> eo a (e <> e')) ee (\_ e' -> co a (e <> e')) ce) ee
      (\a e -> n (\_ e' -> co a (e <> e')) ce (\_ e' -> co a (e <> e')) ce) ce
  {-# INLINE (<*) #-}
  Parser m *> Parser n = Parser $ \ eo ee co ce -> 
    m (\_ e -> n (\a e' -> eo a (e <> e')) ee (\a e' -> co a (e <> e')) ce) ee
      (\_ e -> n (\a e' -> co a (e <> e')) ce (\a e' -> co a (e <> e')) ce) ce
  {-# INLINE (*>) #-}

instance Alt (Parser e) where (<!>) = (<|>)
instance Plus (Parser e) where zero = empty
instance Alternative (Parser e) where
  empty = Parser $ \_ ee _ _ -> ee mempty
  {-# INLINE empty #-}
  -- Parser m <|> Parser n = Parser $ \ eo ee co ce -> m eo (n eo ee co ce) co ce
  Parser m <|> Parser n = Parser $ \ eo ee co ce -> 
    m eo (\e -> n (\a e'-> eo a (e <> e')) (\e' -> ee (e <> e')) co ce) 
      co ce
  {-# INLINE (<|>) #-}
instance Semigroup (Parser e a) where
  (<>) = (<|>) 

instance Monoid (Parser e a) where
  mappend = (<|>)
  mempty = empty

instance Bind (Parser e) where (>>-) = (>>=)
instance Monad (Parser e) where
  return a = Parser $ \ eo _ _ _ -> eo a mempty
  {-# INLINE return #-}
  Parser m >>= k = Parser $ \ eo ee co ce -> 
    m (\a e -> unparser (k a) (\b e' -> eo b (e <> e')) (\e' -> ee (e <> e')) co ce) ee 
      (\a e -> unparser (k a) (\b e' -> co b (e <> e')) (\e' -> ce (e <> e')) co ce) ce
  {-# INLINE (>>=) #-}
  (>>) = (*>) 
  {-# INLINE (>>) #-}
  fail = throwError . FailErr
  {-# INLINE fail #-}

instance MonadPlus (Parser e) where
  mzero = empty
  mplus = (<|>) 

instance MonadWriter (Seq (Diagnostic e)) (Parser e) where
  tell w = Parser $ \eo _ _ _ -> eo () mempty { errLog = w }
  {-# INLINE tell #-}
  listen (Parser m) = Parser $ \eo ee co ce -> 
    m (\ a e -> eo (a,errLog e) e) ee 
      (\ a e -> co (a,errLog e) e) ce 
  pass (Parser m) = Parser $ \eo ee co ce -> 
    m (\(a,p) e -> eo a e { errLog = p $ errLog e }) ee
      (\(a,p) e -> co a e { errLog = p $ errLog e }) ce

logging :: DiagnosticLevel -> e -> Parser e ()
logging level e = do
  m <- mark
  l <- line
  tell $ return $ Diagnostic (rendering m l) level e []

instance MonadDiagnostic e (Parser e) where
  record = tell . return
  fatal e = Parser $ \_ _ _ ce d bs -> ce mempty { errMessage = FatalErr (Diagnostic (rendering d bs) Fatal e []) } d bs
  err e = throwError $ RichErr $ \r -> Diagnostic r Error e []
  warn = logging Warning
  note = logging Note
  verbose = logging . Verbose

instance MonadError (Err e) (Parser e) where
  throwError m = Parser $ \_ ee _ _ -> ee mempty { errMessage = m } 
  {-# INLINE throwError #-}
  catchError (Parser m) k = Parser $ \ eo ee co ce -> 
    m eo (\e -> unparser (k (errMessage e)) (\a e'-> eo a (e <> e')) (\e' -> ee (e <> e')) co ce) 
      co ce
  {-# INLINE catchError #-}

instance MonadParser (Parser e) where
  -- commit (Parser m) = Parser $ \ _ _ co ce -> m co ce co ce
  try (Parser m) = Parser $ \ eo ee co ce -> m eo ee co $ 
    \e -> if fatalErr (errMessage e) then ce e else ee e
  {-# INLINE try #-}
  unexpected s = Parser $ \ _ ee _ _ -> ee mempty { errMessage = UnexpectedErr s } 

  labels (Parser p) msgs = Parser $ \ eo ee -> p
     (\a e -> eo a $ if knownErr (errMessage e) 
                     then e { errExpected = errExpected e }
                     else e)
     (\e -> ee e { errExpected = msgs })
  {-# INLINE labels #-}
  liftIt m = Parser $ \ eo _ _ _ d bs -> do 
     a <- m
     eo a mempty d bs
  {-# INLINE liftIt #-}
  mark = Parser $ \eo _ _ _ d -> eo d mempty d
  {-# INLINE mark #-}
  release d' = Parser $ \_ ee co _ d bs -> do
    mbs <- rewindIt d'
    case mbs of
      Just bs' -> co () mempty d' bs'
      Nothing -> ee mempty d bs
  {-# INLINE release #-}
  line = Parser $ \eo _ _ _ d bs -> eo bs mempty d bs
  {-# INLINE line #-}
  satisfy f = Parser $ \ _ ee co _ d bs ->
    case UTF8.uncons $ Strict.drop (columnByte d) bs of
      Nothing             -> ee mempty { errMessage = EndOfFileErr } d bs
      Just (c, xs) 
        | not (f c)       -> ee mempty d bs
        | Strict.null xs  -> let ddc = d <> delta c in
                             join $ fillIt (co c mempty ddc bs) (co c mempty) ddc
        | otherwise       -> co c mempty (d <> delta c) bs 
  {-# INLINE satisfy #-}
  satisfyAscii f = Parser $ \ _ ee co _ d bs ->
    let b = columnByte d in
    if b >= 0 && b < Strict.length bs 
    then case toEnum $ fromEnum $ Strict.index bs b of
      c | not (f c)                 -> ee mempty d bs
        | b == Strict.length bs - 1 -> let ddc = d <> delta c in 
                                       join $ fillIt (co c mempty ddc bs) (co c mempty) ddc
        | otherwise                 -> co c mempty (d <> delta c) bs
    else ee mempty { errMessage = EndOfFileErr } d bs
  {-# INLINE satisfyAscii #-}

data St e a = JuSt a !(ErrState e) !Delta !ByteString
            | NoSt !(ErrState e) !Delta !ByteString

instance Bifunctor St where
  bimap f g (JuSt a e d bs) = JuSt (g a) (fmap f e) d bs
  bimap f _ (NoSt e d bs) = NoSt (fmap f e) d bs

stepParser :: (Diagnostic e -> Diagnostic t) -> 
              (ErrState e -> Delta -> ByteString -> Diagnostic t) ->
              Parser e a -> Delta -> ByteString -> Step t a
stepParser yl y (Parser p) d0 bs0 = 
  go mempty $ p ju no ju no d0 bs0
  where
    ju a e d bs = Pure (JuSt a e d bs)
    no e d bs = Pure (NoSt e d bs)
    go r (Pure (JuSt a e _ _)) = StepDone r (yl <$> errLog e) a
    go r (Pure (NoSt e d bs))  = StepFail r (yl <$> errLog e) $ y e d bs
    go r (It ma k) = StepCont r (case ma of
                                   JuSt a e _ _ -> Success (yl <$> errLog e) a
                                   NoSt e d bs  -> Failure (yl <$> errLog e) (y e d bs)) 
                                (go <*> k)

why :: Pretty e => (e -> Doc t) -> ErrState e -> Delta -> ByteString -> Diagnostic (Doc t)
why pp (ErrState ss _m _) d bs 
  | Set.null ss = diagnose pp (addCaret d $ rendering d bs) m
  | otherwise   = expected <$> diagnose pp (addCaret d $ rendering d bs) m 
  where
    m = EmptyErr
    expected doc = doc <> text ", expected" <+> fillSep (punctuate (char ',') $ text <$> toList ss)

parseTest :: Show a => Parser TermDoc a -> String -> IO ()
parseTest p s = case starve (feed st (UTF8.fromString s)) of
  Failure xs e -> displayLn $ prettyTerm $ toList (xs |> e)
  Success xs a -> do 
    displayLn $ prettyTerm $ toList xs
    print a
  where st = stepParser id (why id) (release mempty *> p) mempty mempty

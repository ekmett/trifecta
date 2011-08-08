{-# LANGUAGE MultiParamTypeClasses, BangPatterns #-}
module Text.Trifecta.It 
  ( P
  , It(..)
  , input
  , line
  , peekIt
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Data.Semigroup
import Data.Monoid
import Data.FingerTree as FingerTree
import Data.ByteString as Strict
import Data.ByteString.Lazy as Lazy
import Data.ByteString.Lazy.UTF8 as LazyUTF8
import Data.Functor.Bind
import Data.Functor.Plus
import Text.Trifecta.Rope as Rope
import Text.Trifecta.Delta
import Text.Trifecta.Bytes
import Text.Parsec.Prim hiding ((<|>))

type P u = ParsecT Delta u It

-- grab the contents of the line that contains delta
line :: Delta -> P u Strict.ByteString
line d = lift $ Strict.concat
              . Lazy.toChunks
              . Lazy.takeWhile (/= 10)
              . snd <$> peekIt (rewind d)

data It a 
  = Done !Rope !Bool a
  | Fail !Rope !Bool String
  | Cont (Rope -> Bool -> It a)
  
instance Show a => Show (It a) where
  showsPrec d (Done r b a) = showParen (d > 10) $ 
    showString "Done " . showsPrec 11 r . showChar ' ' . showsPrec 11 b . showChar ' ' . showsPrec 11 a
  showsPrec d (Fail r b s) = showParen (d > 10) $
    showString "Fail " . showsPrec 11 r . showChar ' ' . showsPrec 11 b . showChar ' ' . showsPrec 11 s
  showsPrec d (Cont _) = showParen (d > 10) $ showString "Cont ..."

instance Functor It where
  fmap f (Done r b a) = Done r b (f a)
  fmap _ (Fail r b s) = Fail r b s
  fmap f (Cont k) = Cont (\r b -> fmap f (k r b))

instance Apply It where
  (<.>) = (<*>) 

instance Applicative It where
  pure = Done mempty False
  (<*>) = ap

instance Alt It where
  Fail r b _ <!> Cont k = k r b
  Fail r b _ <!> Done _ _ a = Done r b a
  Fail r b _ <!> Fail _ _ s = Fail r b s
  m <!> _ = m

instance Alternative It where
  (<|>) = (<!>)
  empty = fail "empty"

instance Bind It where
  (>>-) = (>>=)

instance Monad It where
  return = Done mempty False
  Done (Rope _ t) False a >>= f | FingerTree.null t = f a
  Done h e a >>= f = case f a of
    Done _ _ b -> Done h e b
    Fail _ _ s -> Fail h e s
    Cont k     -> k h e
  Fail r b s >>= _ = Fail r b s
  Cont k >>= f     = Cont $ \h e -> k h e >>= f
  fail = Fail mempty False

instance MonadPlus It where
  mplus = (<!>) 
  mzero = fail "mzero"

input :: It Rope
input = Cont $ \r e -> Done r e r

instance Stream Delta It Char where
  uncons d = (k <$> peekIt d) <|> return Nothing where 
    k (d', bs) = case LazyUTF8.uncons bs of
      Just (c, _) -> Just (c, d' <> delta c)
      Nothing     -> Nothing

peekIt :: Delta -> It (Delta, Lazy.ByteString)
peekIt n = Cont go where
  go h eof 
    | bytes n < bytes (lastNewline h eof) = grab n h (\c lbs -> Done h eof (c, lbs)) 
                                            (Fail h eof "peek: failed to grab rope")
    | eof                   = Fail h True "Unexpected EOF"
    | otherwise             = Cont $ \h' -> go (h <> h') -- h' <> h

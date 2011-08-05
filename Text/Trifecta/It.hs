{-# LANGUAGE MultiParamTypeClasses #-}
module Text.Trifecta.It 
  ( It(..)
  , getRope
  , getMeasure
  , getEof
  ) where

import Control.Applicative
import Control.Monad
import Data.Semigroup
import Data.Monoid
import Data.Word
import Data.FingerTree as FingerTree
import Data.Functor.Bind
import Data.Functor.Plus
import Text.Trifecta.Cursor
import Text.Trifecta.Rope as Rope
import Text.Trifecta.Delta
import Text.Parsec.Prim hiding ((<|>))

data It a 
  = Done !Rope !Bool a
  | Fail !Rope !Bool String
  | Cont (Rope -> Bool -> It a)
  
instance Show a => Show (It a) where
  showsPrec d (Done r b a) = showParen (d > 10) $ 
    showString "Done " . showsPrec 11 r . showChar ' ' . showsPrec 11 b . showChar ' ' . showsPrec 11 a
  showsPrec d (Fail r b s) = showParen (d > 10) $
    showString "Fail " . showsPrec 11 r . showChar ' ' . showsPrec 11 b . showChar ' ' . showsPrec 11 s
  showsPrec d (Cont k) = showParen (d > 10) $ showString "Cont ..."

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

getRope :: It Rope
getRope = Cont $ \r e -> Done r e r

getMeasure :: It Cursor
getMeasure = Cont $ \r e -> Done r e (measure r)

getEof :: It Bool
getEof = Cont $ \ r e -> Done r e e 

instance Stream Cursor It Char where
  uncons (Cursor b d _ _) = (getWord8 b >>= go) <|> return Nothing where
    go c | c <= 0x7f = do
        Cursor _ _ i t <- getMeasure
        return $ Just (toEnum (fromEnum c), Cursor (b + 1) (d <> delta c) i t)
         | otherwise = error "TODO"
    -- TODO: finish these

getWord8 :: Int -> It Word8
getWord8 n = Cont go where
  go h eof 
    | n < Rope.lastNewline h eof = Done h eof $ indexByte n h
    | eof                        = Fail h True "Unexpected EOF"
    | otherwise                  = Cont $ \h' -> go (h <> h')

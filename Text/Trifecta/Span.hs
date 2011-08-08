module Text.Trifecta.Span
  ( Span(..)
  , HasSpan(..)
  , Spanned(..)
  , spanned
  -- * Internals
  , spanEffects
  , drawSpan
  , addSpan
  ) where

import Control.Applicative
import Data.Hashable
import Data.Semigroup
import Data.Semigroup.Foldable
import Data.Semigroup.Traversable
import Data.Foldable
import Data.Traversable
import Control.Comonad
import Data.Functor.Bind
import Data.ByteString (ByteString)
import Text.Trifecta.Bytes
import Text.Trifecta.Caret
import Text.Trifecta.Delta
import Text.Trifecta.Render
import Text.Trifecta.It
import Text.Trifecta.Util
import Text.Parsec.Prim
import Data.Array
import System.Console.Terminfo.Color
import System.Console.Terminfo.PrettyPrint
import Prelude as P hiding (span)

spanEffects :: [ScopedEffect]
spanEffects  = [soft (Foreground Green)]

drawSpan :: Delta -> Delta -> Delta -> Lines -> Lines
drawSpan s e d a
  | nl && nh  = go (column l) (P.replicate (max (column h   - column l + 1) 0) '~') a
  | nl        = go (column l) (P.replicate (max (snd (snd (bounds a)) - column l + 2) 0) '~') a
  |       nh  = go (-1)       (P.replicate (max (column h + 1) 0) '~') a
  | otherwise = a
  where
    go = draw spanEffects 1
    l = argmin bytes s e
    h = argmax bytes s e
    nl = near l d
    nh = near h d

-- |
-- > int main(int argc, char ** argv) { int; }
-- >                                    ^~~
addSpan :: Delta -> Delta -> Render -> Render
addSpan s e r = drawSpan s e .# r

data Span = Span !Delta !Delta {-# UNPACK #-} !ByteString deriving (Eq,Ord,Show)

instance HasCaret Span where
  caret (Span s _ b) = Caret s b

instance Renderable Span where
  render (Span s e bs) = addSpan s e $ surface s bs

class HasSpan t where
  span :: t -> Span

instance HasSpan Span where
  span = id

instance Semigroup Span where
  Span s _ b <> Span _ e _ = Span s e b


data Spanned a = a :~ Span deriving (Eq,Ord,Show)

instance Functor Spanned where
  fmap f (a :~ s) = f a :~ s

instance Extend Spanned where
  extend f as@(_ :~ s) = f as :~ s

instance Comonad Spanned where
  extract (a :~ _) = a

instance Apply Spanned where
  (f :~ s) <.> (a :~ t) = f a :~ (s <> t)

instance Bind Spanned where
  (a :~ s) >>- f = case f a of
     b :~ t -> b :~ (s <> t)

instance Foldable Spanned where
  foldMap f (a :~ _) = f a 

instance Traversable Spanned where
  traverse f (a :~ s) = (:~ s) <$> f a

instance Foldable1 Spanned where
  foldMap1 f (a :~ _) = f a 

instance Traversable1 Spanned where
  traverse1 f (a :~ s) = (:~ s) <$> f a

instance HasSpan (Spanned a) where
  span (_ :~ c) = c

instance Renderable (Spanned a) where
  render = render . span

instance HasCaret (Spanned a) where
  caret = caret . span

instance Hashable Span where
  hash (Span s e bs) = hash s `hashWithSalt` e `hashWithSalt` bs

instance Hashable a => Hashable (Spanned a) where
  hash (a :~ s) = hash a `hashWithSalt` s

spanned :: P u a -> P u (Spanned a)
spanned p = do
  m <- getInput
  l <- line m
  a <- p
  r <- getInput
  return $ a :~ Span m r l

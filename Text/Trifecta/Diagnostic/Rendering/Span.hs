{-# LANGUAGE MultiParamTypeClasses #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Trifecta.Diagnostic.Rendering.Span
-- Copyright   :  (C) 2011 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
----------------------------------------------------------------------------
module Text.Trifecta.Diagnostic.Rendering.Span
  ( Span(..)
  , span
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
import Data.Semigroup.Reducer
import Data.Semigroup.Foldable
import Data.Semigroup.Traversable
import Data.Foldable
import Data.Traversable
import Control.Comonad
import Data.Functor.Bind
import Data.ByteString (ByteString)
import Text.Trifecta.Rope.Bytes
import Text.Trifecta.Rope.Delta
import Text.Trifecta.Diagnostic.Rendering.Prim
import Text.Trifecta.Util
import Text.Trifecta.Parser.Class
import Data.Array
import System.Console.Terminfo.Color
import System.Console.Terminfo.PrettyPrint
import Prelude as P hiding (span)

spanEffects :: [ScopedEffect]
spanEffects  = [soft (Foreground Green)]

drawSpan :: Delta -> Delta -> Delta -> Lines -> Lines
drawSpan s e d a
  | nl && nh  = go (column l) (rep (max (column h - column l) 0) '~') a
  | nl        = go (column l) (rep (max (snd (snd (bounds a)) - column l + 1) 0) '~') a
  |       nh  = go (-1)       (rep (max (column h + 1) 0) '~') a
  | otherwise = a
  where
    go = draw spanEffects 1 . fromIntegral
    l = argmin bytes s e
    h = argmax bytes s e
    nl = near l d
    nh = near h d
    rep = P.replicate . fromIntegral

-- |
-- > int main(int argc, char ** argv) { int; }
-- >                                    ^~~
addSpan :: Delta -> Delta -> Rendering -> Rendering
addSpan s e r = drawSpan s e .# r

data Span = Span !Delta !Delta {-# UNPACK #-} !ByteString deriving (Eq,Ord,Show)

instance Renderable Span where
  render (Span s e bs) = addSpan s e $ rendering s bs

instance Semigroup Span where
  Span s _ b <> Span _ e _ = Span s e b

instance Reducer Span Rendering where
  unit = render

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

instance Reducer (Spanned a) Rendering where
  unit = render

instance Renderable (Spanned a) where
  render (_ :~ s) = render s

instance Hashable Span where
  hash (Span s e bs) = hash s `hashWithSalt` e `hashWithSalt` bs

instance Hashable a => Hashable (Spanned a) where
  hash (a :~ s) = hash a `hashWithSalt` s

span :: MonadParser m => m a -> m Span 
span p = (\s l e -> Span s e l) <$> mark <*> line <*> (p *> mark)
  
spanned :: MonadParser m => m a -> m (Spanned a)
spanned p = (\s l a e -> a :~ Span s e l) <$> mark <*> line <*> p <*> mark

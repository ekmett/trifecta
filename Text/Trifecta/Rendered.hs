{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FlexibleInstances, BangPatterns, PatternGuards #-}
module Text.Trifecta.Rendered
  ( Rendered(..)
  ) where

import Control.Applicative
import Data.Semigroup
import Data.Semigroup.Foldable
import Data.Semigroup.Traversable
import Data.Foldable
import Data.Traversable
import Control.Comonad
import Data.Functor.Bind
import Text.Trifecta.Delta
import Text.Trifecta.Bytes
import Text.Trifecta.Render
import Prelude hiding (span)

data Rendered a = a :@ Render

instance Functor Rendered where
  fmap f (a :@ s) = f a :@ s

instance HasDelta (Rendered a) where
  delta = delta . render

instance HasBytes (Rendered a) where
  bytes = bytes . delta

instance Extend Rendered where
  extend f as@(_ :@ s) = f as :@ s

instance Comonad Rendered where
  extract (a :@ _) = a

instance Apply Rendered where
  (f :@ s) <.> (a :@ t) = f a :@ (s <> t)

instance Bind Rendered where
  (a :@ s) >>- f = case f a of
     b :@ t -> b :@ (s <> t)

instance Foldable Rendered where
  foldMap f (a :@ _) = f a 

instance Traversable Rendered where
  traverse f (a :@ s) = (:@ s) <$> f a

instance Foldable1 Rendered where
  foldMap1 f (a :@ _) = f a 

instance Traversable1 Rendered where
  traverse1 f (a :@ s) = (:@ s) <$> f a

instance Renderable (Rendered a) where
  render (_ :@ s) = s

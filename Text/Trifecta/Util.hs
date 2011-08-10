module Text.Trifecta.Util 
  ( argmin
  , argmax
  ) where

import Control.Applicative
import Data.Bifunctor
import Data.Bifoldable
import Data.Bitraversable
import Data.Foldable
import Data.Traversable

argmin :: Ord b => (a -> b) -> a -> a -> a
argmin f a b
  | f a <= f b = a
  | otherwise = b
{-# INLINE argmin #-}

argmax :: Ord b => (a -> b) -> a -> a -> a
argmax f a b
  | f a > f b = a
  | otherwise = b
{-# INLINE argmax #-}


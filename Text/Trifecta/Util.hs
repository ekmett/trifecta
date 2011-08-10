module Text.Trifecta.Util 
  ( argmin
  , argmax
  ) where

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


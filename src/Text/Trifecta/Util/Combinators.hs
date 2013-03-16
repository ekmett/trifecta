-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Trifecta.Util.Combinators
-- Copyright   :  (C) 2011-2013 Edward Kmett,
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
----------------------------------------------------------------------------
module Text.Trifecta.Util.Combinators
  ( argmin
  , argmax
  -- * Text conversions
  , fromLazy
  , toLazy
  , takeLine
  -- * Misc.
  , (<$!>)
  ) where

import Control.Lens
import Data.Text.Lazy as Lazy
import Data.Text as Strict

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

fromLazy :: Lazy.Text -> Strict.Text
fromLazy = Lazy.toStrict
{-# INLINE fromLazy #-}

toLazy :: Strict.Text -> Lazy.Text
toLazy = Lazy.fromStrict
{-# INLINE toLazy #-}

takeLine :: Lazy.Text -> Lazy.Text
takeLine s = case s^?each.filtered (=='\n').asIndex of
  Just i -> Lazy.take (i + 1) s
  Nothing -> s
-- TODO: speed this up!
{-# INLINE takeLine #-}

infixl 4 <$!>
(<$!>) :: Monad m => (a -> b) -> m a -> m b
f <$!> m = do
  a <- m
  return $! f a

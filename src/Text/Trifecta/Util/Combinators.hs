-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2011-2019 Edward Kmett
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
  -- * ByteString conversions
  , fromLazy
  , toLazy
  , takeLine
  , (<$!>)
  ) where

import Data.ByteString      as Strict
import Data.ByteString.Lazy as Lazy

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

fromLazy :: Lazy.ByteString -> Strict.ByteString
fromLazy = Strict.concat . Lazy.toChunks

toLazy :: Strict.ByteString -> Lazy.ByteString
toLazy = Lazy.fromChunks . return

takeLine :: Lazy.ByteString -> Lazy.ByteString
takeLine s = case Lazy.elemIndex 10 s of
  Just i -> Lazy.take (i + 1) s
  Nothing -> s

infixl 4 <$!>
(<$!>) :: Monad m => (a -> b) -> m a -> m b
f <$!> m = do
  a <- m
  return $! f a

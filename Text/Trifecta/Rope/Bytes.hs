-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Trifecta.Rope.Bytes
-- Copyright   :  (C) 2011 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
----------------------------------------------------------------------------
module Text.Trifecta.Rope.Bytes
  ( HasBytes(..)
  ) where

import Data.ByteString as Strict
import Data.FingerTree
import Data.Int (Int64)

class HasBytes t where
  bytes :: t -> Int64

instance HasBytes ByteString where
  bytes = fromIntegral . Strict.length

instance (Measured v a, HasBytes v) => HasBytes (FingerTree v a) where
  bytes = bytes . measure

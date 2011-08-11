module Text.Trifecta.Rope.Bytes
  ( HasBytes(..)
  ) where

import Data.ByteString as Strict
import Data.FingerTree

class HasBytes t where
  bytes :: t -> Int

instance HasBytes ByteString where
  bytes = Strict.length

instance (Measured v a, HasBytes v) => HasBytes (FingerTree v a) where
  bytes = bytes . measure

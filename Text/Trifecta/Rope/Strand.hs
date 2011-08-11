{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FlexibleInstances #-}
module Text.Trifecta.Rope.Strand
  ( Strand(..)
  ) where

import Data.Interned
import Data.Hashable
import Data.ByteString as Strict
import Data.FingerTree as FingerTree
import Text.Trifecta.Rope.Hunk
import Text.Trifecta.Rope.Path
import Text.Trifecta.Rope.Bytes
import Text.Trifecta.Rope.Delta

data Strand
  = HunkStrand !Hunk
  | PathStrand !Path
  deriving Show

--instance Show Strand where
--  showsPrec d (HunkStrand h) = showsPrec d h
--  showsPrec d (PathStrand p) = showsPrec d p

instance Measured Delta Strand where
  measure (HunkStrand s) = delta s
  measure (PathStrand p) = delta p

instance Hashable Strand where
  hash (HunkStrand h) = hashWithSalt 0 h
  hash (PathStrand p) = hashWithSalt 0 p

instance HasDelta Strand where
  delta = measure

instance HasBytes Strand where
  bytes (HunkStrand h) = Strict.length (unintern h)
  bytes _ = 0

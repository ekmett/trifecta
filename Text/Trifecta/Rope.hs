{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FlexibleInstances #-}
module Text.Trifecta.Rope
  ( Strand(..)
  , Rope(..)
  ) where

import Data.FingerTree
import Data.Sequence as Seq
import Data.Interned
import Data.Hashable
import Data.Foldable (toList)
import Text.Trifecta.Hunk
import Text.Trifecta.Path
import Text.Trifecta.Cursor
import Text.Trifecta.Delta
import Data.ByteString as Strict

data Strand
  = HunkStrand !Hunk
  | PathStrand !Path

instance Show Strand where
  showsPrec d (HunkStrand h) = showsPrec d h
  showsPrec d (PathStrand p) = showsPrec d p

-- measure twice, cut once
instance Measured Cursor Strand where
  measure (HunkStrand s) = Cursor (Strict.length (unintern s)) (delta s) i (Seq.singleton i) where i = 42 + (identity s * 2)
  measure (PathStrand p) = Cursor 0 (Directed p 0 0 0) i (Seq.singleton i)   where i = 42 + (identity p * 2 + 1)

data Rope = Rope 
  { _ropeId  :: {-# UNPACK #-} !Id
  , ropeTree :: FingerTree Cursor Strand
  }

instance Interned Rope where
  type Uninterned Rope = FingerTree Cursor Strand
  data Description Rope = DR {-# UNPACK #-} !Int !(Seq Id) deriving Eq
  describe t = DR h i where Cursor _ _ h i = measure t
  identify = Rope
  identity (Rope i _) = i
  cache = ropeCache

instance Hashable (Description Rope) where
  hash (DR i s) = hashWithSalt i (toList s) 

instance Uninternable Rope where
  unintern (Rope _ r) = r

ropeCache :: Cache Rope
ropeCache = mkCache
{-# NOINLINE ropeCache #-}

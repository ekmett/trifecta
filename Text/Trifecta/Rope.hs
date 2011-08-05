{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FlexibleInstances #-}
module Text.Trifecta.Rope
  ( Strand(..)
  , Rope(..)
  , indexByte
  , size
  , lastNewline
  ) where

import Data.Sequence as Seq
import Data.Interned
import Data.Hashable
import Data.Monoid
import Data.Semigroup
import Data.ByteString as Strict
import Data.FingerTree as FingerTree
import Data.Word
import Data.Foldable (toList)
import Text.Trifecta.Hunk
import Text.Trifecta.Path
import Text.Trifecta.Cursor
import Text.Trifecta.Delta

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
  , ropeTree :: !(FingerTree Cursor Strand)
  }

size :: Rope -> Int
size = cursorBytes . measure

instance Measured Cursor Rope where
  measure (Rope _ t) = measure t

lastNewline :: Rope -> Bool -> Int
lastNewline t True  = size t
lastNewline t False = case d of
	Columns _ _       -> 0
        Tab _ _ _         -> 0
        Lines _ _ b'      -> b - b'
        Directed _ _ _ b' -> b - b'
  where Cursor b d _ _ = measure t

indexByte :: Int -> Rope -> Word8
indexByte i (Rope _ t) = Strict.index a $ i - cursorBytes (measure l) where
   (l, r) = FingerTree.split (\b -> cursorBytes b > i) t
   HunkStrand (Hunk _ _ a) FingerTree.:< _ = FingerTree.viewl r

instance Monoid Rope where
  mempty = intern mempty
  mappend x y = intern (unintern x `mappend` unintern y)

instance Semigroup Rope where
  x <> y = intern (unintern x `mappend` unintern y)

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

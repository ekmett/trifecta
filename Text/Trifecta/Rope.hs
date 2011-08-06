{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FlexibleInstances, BangPatterns, PatternGuards #-}
module Text.Trifecta.Rope
  ( Strand(..)
  , Rope(..)
  , indexByte
  , dropRope
  , lastNewline
  ) where

import Data.Interned
import Data.Hashable
import Data.Monoid
import Data.Semigroup
import Data.Semigroup.Reducer
import Data.Semigroup.Reducer.With
import qualified Data.ByteString as Strict
import Data.FingerTree as FingerTree
import Data.Word
import Data.Foldable (toList)
import Text.Trifecta.Hunk
import Text.Trifecta.Path
import Text.Trifecta.Cursor
import Text.Trifecta.Strand
import Text.Trifecta.Tally

data Rope = Rope 
  { _ropeId     :: {-# UNPACK #-} !Id
  , ropeMeasure :: !Summary
  , ropeTree    :: !(FingerTree Delta Strand)
  }

instance Interned Rope where
  type Uninterned Rope = FingerTree Summary Strand
  newtype Description Rope = DR Sids deriving Eq
  describe = DR . sids
  identify i t = Rope i (measure t) t
  identity (Rope i _ _) = i
  cache = ropeCache

instance Hashable (Description Rope) where
  hash (DR s) = hash (tallySum (tally s))

instance Uninternable Rope where
  unintern (Rope _ r) = r

ropeCache :: Cache Rope
ropeCache = mkCache
{-# NOINLINE ropeCache #-}

instance HasBytes Rope where
  bytes = bytes . measure

instance HasDelta Rope where
  delta = delta . measure

instance HasSummary Rope where
  summary = measure

instance Measured Summary Rope where
  measure (Rope _ s _) = s

instance Reducer Strand Rope where
  unit !s = intern (singleton s)
  cons !s (Rope _ _ t) = intern (s <| t)
  snoc (Rope _ _ t) !s = intern (t |> s)

instance Reducer Hunk Rope where
  unit = unit . HunkStrand
  cons = cons . HunkStrand
  snoc r = snoc r . HunkStrand
  
instance Reducer Path Rope where
  unit = unit . PathStrand
  cons = cons . PathStrand
  snoc r = snoc r . PathStrand

instance Reducer Strict.ByteString Rope where
  unit = unit . hunk
  cons = cons . hunk 
  snoc r = snoc r . hunk

instance Show Rope where
  showsPrec d (Rope _ _ r) = showsPrec d (toList r)

-- | obtain the byte location of the last newline in a rope, or the end of the rope if at EOF
lastNewline :: Rope -> Bool -> Delta
lastNewline t True  = delta t
lastNewline t False = rewind (delta t)

-- | grab the remainder of a bytestring from some point and the uninterned tail of the rope
--   and if the index is to the start of a bytestring fragment, we reset the cursor to deal with
--   path fragments
grab :: Delta -> Rope -> (Delta -> Lazy.ByteString -> r) -> r -> r
grab i t ks kf 
    case FingerTree.viewl r of
      HunkStrand (Hunk _ _ a) :< r' -> case bi - bl of 
        0 | Cursor b d _ <- measure l -> ks (Cursor b d (cursorIndex (measure t))) 
                                            (Lazy.fromChunks $ a : chunks r')
        db -> ks i (Lazy.fromChunks $ (Strict.drop db a) : chunks r')
      _ -> kf
  where 
    (l, r) = FingerTree.split (\b -> bytes b > bi) (unintern t)
    bi = bytes i
    bl = bytes l
    chunks t = case viewl t of 
      HunkStrand (Hunk _ _ a) :< t' -> a : chunks t'
      _ -> []

-- | attempting to index out of range will result in an error
indexByte :: Int -> Rope -> Word8
indexByte i (Rope _ t) = Strict.index a $ i - cursorBytes (measure l) where
   (l, r) = FingerTree.split (\b -> cursorBytes b > i) t
   HunkStrand (Hunk _ _ a) :< _ = FingerTree.viewl r

instance Monoid Rope where
  mempty = intern mempty
  mappend x y = intern (unintern x `mappend` unintern y)

instance Semigroup Rope where
  x <> y = intern (unintern x `mappend` unintern y)


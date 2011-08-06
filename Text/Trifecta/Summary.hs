{-# LANGUAGE PatternGuards #-}
module Text.Trifecta.Summary 
  ( Summary(..)
  , HasSummary(..)
  ) where

import Data.Hashable
import Data.Monoid
import Data.Semigroup
import Data.Semigroup.Reducer.With
import Data.Foldable
import Data.FingerTree
import Text.Trifecta.Delta
import Text.Trifecta.Tally
import Text.Trifecta.Strand

-- | invariant, the summary was obtained by summing the deltas of each Strand
data Summary = Summary !Delta !Sids deriving Show

instance HasDelta Summary where
  delta (Summary d _) = d

instance HasSids Summary where
  sids (Summary _ q) = q

instance HasBytes Summary where
  bytes = bytes  . delta

class HasSummary t where
  summary :: t -> Summary

instance HasSummary Summary where
  summary = id

instance (Measured v a, HasSummary v) => HasSummary (FingerTree v a) where
  summary = summary . measure

-- instance Eq Summary where (==) = (==) `on` delta
-- instance Ord Summary where compare = compare `on` delta
-- instance Hashable Summary where hash = hash . delta

instance Monoid Summary where
  mempty = Summary mempty mempty
  Summary d q `mappend` Summary d' q' = Summary (d <> d') (q <> q')

instance Semigroup Summary where
  Summary d q <> Summary d' q' = Summary (d <> d') (q <> q')

instance HasTally Summary where
  tally = tally . sids

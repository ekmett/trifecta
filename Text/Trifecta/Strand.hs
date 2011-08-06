{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FlexibleInstances #-}
module Text.Trifecta.Strand
  ( Strand(..)
  ) where

import Data.Interned
import Data.Hashable
import Data.Semigroup.Reducer.With
import Data.ByteString as Strict
import Data.FingerTree as FingerTree
import Text.Trifecta.Sid
import Text.Trifecta.Bytes
import Text.Trifecta.Hunk
import Text.Trifecta.Path
import Text.Trifecta.Summary
import Text.Trifecta.Delta
import Text.PrettyPrint.Leijen.Extras

data Strand
  = HunkStrand !Hunk
  | PathStrand !Path

instance Show Strand where
  showsPrec d (HunkStrand h) = showsPrec d h
  showsPrec d (PathStrand p) = showsPrec d p

instance Pretty Strand where
  pretty (HunkStrand h) = pretty h
  pretty (PathStrand p) = pretty p

instance HasSid Strand where 
  sid (HunkStrand h) = sid h
  sid (PathStrand p) = sid p

instance HasSids Strand where
  sids (HunkStrand h) = sids h
  sids (PathStreand p) = sids p

instance Measured Summary Strand where
  measure (HunkStrand s) = Summary (delta s) (sids s)
  measure (PathStrand p) = Summary (delta p) (sids p)

instance Hashable Strand where
  hash = strandId 

instance HasDelta Strand where
  delta = measure

instance HasBytes Strand where
  bytes (HunkStrand h) = Strict.length (unintern h)
  bytes _ = 0

{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FlexibleInstances, BangPatterns, PatternGuards #-}
module Text.Trifecta.Rope
  ( Rope(..)
  , rope
  , strands
  , grab
  , lastNewline
  ) where

import Data.Monoid
import Data.Semigroup
import Data.Semigroup.Reducer
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Lazy as Lazy
import Data.FingerTree as FingerTree
import Data.Foldable (toList)
import Text.Trifecta.Hunk
import Text.Trifecta.Path
import Text.Trifecta.Delta
import Text.Trifecta.Bytes
import Text.Trifecta.Strand

data Rope = Rope !Delta !(FingerTree Delta Strand) deriving Show

rope :: FingerTree Delta Strand -> Rope
rope r = Rope (measure r) r

strands :: Rope -> FingerTree Delta Strand
strands (Rope _ r) = r

instance HasBytes Rope where
  bytes = bytes . measure

instance HasDelta Rope where
  delta = measure

instance Measured Delta Rope where
  measure (Rope s _) = s

-- | obtain the byte location of the last newline in a rope, or the end of the rope if at EOF
lastNewline :: Rope -> Bool -> Delta
lastNewline t True  = delta t
lastNewline t False = rewind (delta t)

-- | grab a lazy bytestring starting from some point. This bytestring does not cross path nodes
--   if the index is to the start of a bytestring fragment, we update it to deal with any 
--   intervening path fragments
grab :: Delta -> Rope -> (Delta ->  Lazy.ByteString -> r) -> r -> r
grab i t ks kf = trim (toList r) (delta l) (bytes i - bytes l) where
  trim (PathStrand p : xs)            j k = trim xs (j <> delta p) k
  trim (HunkStrand (Hunk _ _ h) : xs) j 0 = go j h xs
  trim (HunkStrand (Hunk _ _ h) : xs) _ k = go i (Strict.drop k h) xs
  trim [] _ _                             = kf
  go j h s = ks j $ Lazy.fromChunks $ h : [ a | HunkStrand (Hunk _ _ a) <- s ]
  (l, r) = FingerTree.split (\b -> bytes b > bytes i) (strands t)

instance Monoid Rope where
  mempty = Rope mempty mempty
  mappend = (<>)

instance Semigroup Rope where
  Rope mx x <> Rope my y = Rope (mx <> my) (x `mappend` y)

instance Reducer Rope Rope where
  unit = id

instance Reducer Strand Rope where
  unit s = rope (singleton s)
  cons s (Rope mt t) = Rope (delta s `mappend` mt) (s <| t)
  snoc (Rope mt t) !s = Rope (mt `mappend` delta s) (t |> s)

instance Reducer Hunk Rope where
  unit s = Rope (delta s) (singleton (HunkStrand s))
  cons s (Rope mt t) = Rope (delta s `mappend` mt) (HunkStrand s <| t)
  snoc (Rope mt t) s = Rope (mt `mappend` delta s) (t |> HunkStrand s)
  
instance Reducer Path Rope where
  unit s = Rope (delta s) (singleton (PathStrand s))
  cons s (Rope mt t) = Rope (delta s `mappend` mt) (PathStrand s <| t)
  snoc (Rope mt t) s = Rope (mt `mappend` delta s) (t |> PathStrand s)

instance Reducer Strict.ByteString Rope where
  unit = unit . hunk
  cons = cons . hunk 
  snoc r = snoc r . hunk

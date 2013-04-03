{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FlexibleInstances, BangPatterns, PatternGuards, DeriveDataTypeable, DeriveGeneric #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Trifecta.Rope
-- Copyright   :  (C) 2011-2013 Edward Kmett,
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
----------------------------------------------------------------------------
module Text.Trifecta.Rope
  ( Rope(..)
  , rope
  , Strand(..)
  , strand
  , strands
  , grabRest
  , grabLine
  ) where

import Data.Semigroup
import Data.Semigroup.Reducer
import Data.Text (Text)
import qualified Data.Text as Strict
import qualified Data.Text.Lazy as Lazy
import Data.FingerTree as FingerTree
import GHC.Generics
import Data.Foldable (toList)
import Data.Hashable
import Text.Trifecta.Util.Combinators as Util
import Text.Trifecta.Delta
import Data.Data

data Strand
  = Strand        {-# UNPACK #-} !Text !Delta
  | Skipping !Delta
  deriving (Show, Data, Typeable, Generic)

strand :: Text -> Strand
strand bs = Strand bs (delta bs)

instance Measured Delta Strand where
  measure (Strand _ s) = delta s
  measure (Skipping d) = d

instance Hashable Strand

instance HasDelta Strand where
  delta = measure

instance HasUnits Strand where
  units (Strand _ d) = units d
  units _            = 0

data Rope = Rope !Delta !(FingerTree Delta Strand) deriving Show

rope :: FingerTree Delta Strand -> Rope
rope r = Rope (measure r) r

strands :: Rope -> FingerTree Delta Strand
strands (Rope _ r) = r

-- | grab a the contents of a rope from a given location up to a newline
grabRest :: Delta -> Rope -> r -> (Delta -> Lazy.Text -> r) -> r
grabRest i t kf ks = trim (delta l) (units i - units l) (toList r) where
  trim j 0 (Strand h _ : xs) = go j h xs
  trim _ k (Strand h _ : xs) = go i (Strict.drop (fromIntegral k) h) xs
  trim j k (p          : xs) = trim (j <> delta p) k xs 
  trim _ _ []                = kf
  go j h s = ks j $ Lazy.fromChunks $ h : [ a | Strand a _ <- s ]
  (l, r) = FingerTree.split (\b -> units b > units i) $ strands t

-- | grab a the contents of a rope from a given location up to a newline
grabLine :: Delta -> Rope -> r -> (Delta -> Strict.Text -> r) -> r
grabLine i t kf ks = grabRest i t kf $ \c -> ks c . Util.fromLazy . Util.takeLine

instance HasUnits Rope where
  units = units . measure

instance HasDelta Rope where
  delta = measure

instance Measured Delta Rope where
  measure (Rope s _) = s

instance Monoid Rope where
  mempty = Rope mempty mempty
  mappend = (<>)

instance Semigroup Rope where
  Rope mx x <> Rope my y = Rope (mx <> my) (x `mappend` y)

instance Reducer Rope Rope where
  unit = id

instance Reducer Strand Rope where
  unit s = rope (FingerTree.singleton s)
  cons s (Rope mt t) = Rope (delta s `mappend` mt) (s <| t)
  snoc (Rope mt t) !s = Rope (mt `mappend` delta s) (t |> s)

instance Reducer Strict.Text Rope where
  unit = unit . strand
  cons = cons . strand
  snoc r = snoc r . strand

instance Reducer [Char] Rope where
  unit = unit . strand . Strict.pack
  cons = cons . strand . Strict.pack
  snoc r = snoc r . strand . Strict.pack

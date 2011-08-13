{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FlexibleInstances, BangPatterns, PatternGuards #-}
module Text.Trifecta.Rope.Prim
  ( Rope(..)
  , rope
  , Strand(..)
  , strand
  , strands
  , grabRest
  , grabLine
  ) where

import Data.Monoid
import Data.Semigroup
import Data.Semigroup.Reducer
import Data.ByteString (ByteString)
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.ByteString.UTF8 as UTF8
import Data.FingerTree as FingerTree
import Data.Foldable (toList)
import Data.Hashable
import Text.Trifecta.Util as Util
import Text.Trifecta.Rope.Bytes
import Text.Trifecta.Rope.Delta

data Strand
  = Strand        {-# UNPACK #-} !ByteString !Delta
  | LineDirective {-# UNPACK #-} !ByteString {-# UNPACK #-} !Int
  deriving Show

strand :: ByteString -> Strand
strand bs = Strand bs (delta bs)

instance Measured Delta Strand where
  measure (Strand _ s) = delta s
  measure (LineDirective p l) = delta (Directed p l 0 0 0)

instance Hashable Strand where
  hash (Strand h _) = hashWithSalt 0 h
  hash (LineDirective p l)   = l `hashWithSalt` p

instance HasDelta Strand where
  delta = measure

instance HasBytes Strand where
  bytes (Strand _ d) = bytes d
  bytes _            = 0

data Rope = Rope !Delta !(FingerTree Delta Strand) deriving Show

rope :: FingerTree Delta Strand -> Rope
rope r = Rope (measure r) r

strands :: Rope -> FingerTree Delta Strand
strands (Rope _ r) = r

-- | grab a the contents of a rope from a given location up to a newline
grabRest :: Delta -> Rope -> r -> (Delta -> Lazy.ByteString -> r) -> r
grabRest i t kf ks = trim (delta l) (bytes i - bytes l) (toList r) where
  trim j 0 (Strand h _ : xs) = go j h xs
  trim _ k (Strand h _ : xs) = go i (Strict.drop k h) xs
  trim j k (p          : xs) = trim (j <> delta p) k xs 
  trim _ _ []                = kf
  go j h s = ks j $ Lazy.fromChunks $ h : [ a | Strand a _ <- s ]
  (l, r) = FingerTree.split (\b -> bytes b > bytes i) $ strands t

-- | grab a the contents of a rope from a given location up to a newline
grabLine :: Delta -> Rope -> r -> (Delta -> Strict.ByteString -> r) -> r
grabLine i t kf ks = grabRest i t kf $ \c -> ks c . Util.fromLazy . Util.takeLine

instance HasBytes Rope where
  bytes = bytes . measure

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

instance Reducer Strict.ByteString Rope where
  unit = unit . strand
  cons = cons . strand
  snoc r = snoc r . strand

instance Reducer [Char] Rope where
  unit = unit . strand . UTF8.fromString
  cons = cons . strand . UTF8.fromString
  snoc r = snoc r . strand . UTF8.fromString



{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeFamilies #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Trifecta.IntervalMap
-- Copyright   :  (c) Edward Kmett 2011
--                (c) Ross Paterson 2008
-- License     :  BSD-style
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable (MPTCs, type families, functional dependencies)
--
-- Interval maps implemented using the 'FingerTree' type, following
-- section 4.8 of
--
--    * Ralf Hinze and Ross Paterson,
--      \"Finger trees: a simple general-purpose data structure\",
--      /Journal of Functional Programming/ 16:2 (2006) pp 197-217.
--      <http://www.soi.city.ac.uk/~ross/papers/FingerTree.html>
--
-- An amortized running time is given for each operation, with /n/
-- referring to the size of the priority queue.  These bounds hold even
-- in a persistent (shared) setting.
--
-- /Note/: Many of these operations have the same names as similar
-- operations on lists in the "Prelude".  The ambiguity may be resolved
-- using either qualification or the @hiding@ clause.
--
-- Unlike "Data.IntervalMap.FingerTree", this version sorts things so
-- that the largest interval from a given point comes first. This way
-- if you have nested intervals, you get the outermost interval before 
-- the contained intervals.
-----------------------------------------------------------------------------

module Text.Trifecta.IntervalMap 
  (
  -- * Intervals
    Interval(..)
  -- * Interval maps
  , IntervalMap(..), singleton, insert
  -- * Searching
  , search, intersections, dominators
  -- * Prepending an offset onto every interval in the map
  , offset
  -- * The result monoid
  , IntInterval(..)
  , fromList
  ) where

import Control.Applicative hiding (empty)
import qualified Data.FingerTree as FT
import Data.FingerTree (FingerTree, Measured(..), ViewL(..), (<|), (><))
import Data.Functor.Plus

import Data.Traversable (Traversable(traverse))
import Data.Foldable (Foldable(foldMap))
import Data.Bifunctor
import Data.Semigroup
import Data.Semigroup.Reducer
import Data.Semigroup.Union
import Data.Semigroup.Foldable
import Data.Semigroup.Traversable
import Data.Key
import Data.Pointed

----------------------------------
-- 4.8 Application: interval trees
----------------------------------

-- | A closed interval.  The lower bound should be less than or equal
-- to the higher bound.
data Interval v = Interval { low :: v, high :: v }
  deriving Show

instance Ord v => Semigroup (Interval v) where
  Interval a b <> Interval c d = Interval (min a c) (max b d)

-- assumes the monoid and ordering are compatible.
instance (Ord v, Monoid v) => Reducer v (Interval v) where 
  unit v = Interval v v
  cons v (Interval a b) = Interval (v `mappend` a) (v `mappend` b)
  snoc (Interval a b) v = Interval (a `mappend` v) (b `mappend` v)

instance Eq v => Eq (Interval v) where
  Interval a b == Interval c d = a == c && d == b

instance Ord v => Ord (Interval v) where
  compare (Interval a b) (Interval c d) = case compare a c of
    LT -> LT
    EQ -> compare d b -- reversed to put larger intervals first
    GT -> GT

instance Functor Interval where
  fmap f (Interval a b) = Interval (f a) (f b)

instance Foldable Interval where
  foldMap f (Interval a b) = f a `mappend` f b

instance Traversable Interval where
  traverse f (Interval a b) = Interval <$> f a <*> f b

instance Foldable1 Interval where
  foldMap1 f (Interval a b) = f a <> f b

instance Traversable1 Interval where
  traverse1 f (Interval a b) = Interval <$> f a <.> f b

instance Pointed Interval where
  point v = Interval v v 

data Node v a = Node (Interval v) a

type instance Key (Node v) = Interval v

instance Functor (Node v) where
  fmap f (Node i x) = Node i (f x)

instance Bifunctor Node where
  bimap f g (Node v a) = Node (fmap f v) (g a)

instance Keyed (Node v) where
  mapWithKey f (Node i x) = Node i (f i x)

instance Foldable (Node v) where
  foldMap f (Node _ x) = f x

instance FoldableWithKey (Node v) where
  foldMapWithKey f (Node k v) = f k v

instance Traversable (Node v) where
  traverse f (Node i x) = Node i <$> f x

instance TraversableWithKey (Node v) where
  traverseWithKey f (Node i x) = Node i <$> f i x

instance Foldable1 (Node v) where
  foldMap1 f (Node _ x) = f x

instance FoldableWithKey1 (Node v) where
  foldMapWithKey1 f (Node k v) = f k v

instance Traversable1 (Node v) where
  traverse1 f (Node i x) = Node i <$> f x

instance TraversableWithKey1 (Node v) where
  traverseWithKey1 f (Node i x) = Node i <$> f i x

-- rightmost interval (including largest lower bound) and largest upper bound.
data IntInterval v = NoInterval | IntInterval (Interval v) v

instance Ord v => Monoid (IntInterval v) where
  mempty = NoInterval
  NoInterval `mappend` i  = i
  i `mappend` NoInterval  = i
  IntInterval _ hi1 `mappend` IntInterval int2 hi2 =
    IntInterval int2 (max hi1 hi2)

instance Ord v => Measured (IntInterval v) (Node v a) where
  measure (Node i _) = IntInterval i (high i)

-- | Map of closed intervals, possibly with duplicates.
-- The 'Foldable' and 'Traversable' instances process the intervals in
-- lexicographical order.
newtype IntervalMap v a = IntervalMap { runIntervalMap :: FingerTree (IntInterval v) (Node v a) } 
-- ordered lexicographically by interval

type instance Key (IntervalMap v) = Interval v

instance Functor (IntervalMap v) where
  fmap f (IntervalMap t) = IntervalMap (FT.unsafeFmap (fmap f) t)

instance Keyed (IntervalMap v) where
  mapWithKey f (IntervalMap t) = IntervalMap (FT.unsafeFmap (mapWithKey f) t)

instance Foldable (IntervalMap v) where
  foldMap f (IntervalMap t) = foldMap (foldMap f) t

instance FoldableWithKey (IntervalMap v) where
  foldMapWithKey f (IntervalMap t) = foldMap (foldMapWithKey f) t 

instance Traversable (IntervalMap v) where
  traverse f (IntervalMap t) =
     IntervalMap <$> FT.unsafeTraverse (traverse f) t

instance TraversableWithKey (IntervalMap v) where
  traverseWithKey f (IntervalMap t) = 
     IntervalMap <$> FT.unsafeTraverse (traverseWithKey f) t

instance Ord v => Measured (IntInterval v) (IntervalMap v a) where
  measure (IntervalMap m) = measure m

largerError :: a
largerError = error "Text.Trifecta.IntervalMap.larger: the impossible happened"

-- | /O(m log (n/\//m))/.  Merge two interval maps.
-- The map may contain duplicate intervals; entries with equal intervals
-- are kept in the original order.
instance Ord v => HasUnion (IntervalMap v a) where
  union (IntervalMap xs) (IntervalMap ys) = IntervalMap (merge1 xs ys) where 
    merge1 as bs = case FT.viewl as of
      EmptyL -> bs
      a@(Node i _) :< as' -> l >< a <| merge2 as' r
        where 
          (l, r) = FT.split larger bs
          larger (IntInterval k _) = k >= i
          larger _ = largerError
    merge2 as bs = case FT.viewl bs of
      EmptyL -> as
      b@(Node i _) :< bs' -> l >< b <| merge1 r bs'
        where 
          (l, r) = FT.split larger as
          larger (IntInterval k _) = k >= i
          larger _ = largerError

instance Ord v => HasUnion0 (IntervalMap v a) where
  empty = IntervalMap FT.empty

instance Ord v => Monoid (IntervalMap v a) where
  mempty = empty
  mappend = union

instance Ord v => Alt (IntervalMap v) where
  (<!>) = union

instance Ord v => Plus (IntervalMap v) where
  zero = empty

-- | /O(n)/. Add a delta to each interval in the map
offset :: (Ord v, Monoid v) => v -> IntervalMap v a -> IntervalMap v a 
offset v (IntervalMap m) = IntervalMap $ FT.fmap' (first (mappend v)) m

-- | /O(1)/.  Interval map with a single entry.
singleton :: Ord v => Interval v -> a -> IntervalMap v a
singleton i x = IntervalMap (FT.singleton (Node i x))

-- | /O(log n)/.  Insert an interval into a map.
-- The map may contain duplicate intervals; the new entry will be inserted
-- before any existing entries for the same interval.
insert :: Ord v => v -> v -> a -> IntervalMap v a -> IntervalMap v a
insert lo hi _ m | lo > hi = m
insert lo hi x (IntervalMap t) = IntervalMap (l >< Node i x <| r) where 
  i = Interval lo hi
  (l, r) = FT.split larger t
  larger (IntInterval k _) = k >= i
  larger _ = largerError

-- | /O(k log (n/\//k))/.  All intervals that contain the given interval,
-- in lexicographical order.
dominators :: Ord v => v -> v -> IntervalMap v a -> [(Interval v, a)]
dominators i j = intersections j i 

-- | /O(k log (n/\//k))/.  All intervals that contain the given point,
-- in lexicographical order.
search :: Ord v => v -> IntervalMap v a -> [(Interval v, a)]
search p = intersections p p

-- | /O(k log (n/\//k))/.  All intervals that intersect with the given
-- interval, in lexicographical order.
intersections :: Ord v => v -> v -> IntervalMap v a -> [(Interval v, a)]
intersections lo hi (IntervalMap t) = matches (FT.takeUntil (greater hi) t) where 
  matches xs  =  case FT.viewl (FT.dropUntil (atleast lo) xs) of
    EmptyL -> []
    Node i x :< xs'  ->  (i, x) : matches xs'

atleast :: Ord v => v -> IntInterval v -> Bool
atleast k (IntInterval _ hi) = k <= hi
atleast _ _ = False

greater :: Ord v => v -> IntInterval v -> Bool
greater k (IntInterval i _) = low i > k
greater _ _ = False

fromList :: Ord v => [(v, v, a)] -> IntervalMap v a
fromList = foldr ins empty where 
  ins (lo, hi, n) = insert lo hi n


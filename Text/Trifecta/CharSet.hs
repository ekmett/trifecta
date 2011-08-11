{-# LANGUAGE BangPatterns, MagicHash #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Trifecta.CharSet
-- Copyright   :  (c) Edward Kmett 2010-2011, 
--                Bryan O'Sullivan 2008
-- License     :  BSD3
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable (Data, BangPatterns, MagicHash)
--
-- Fast set membership tests for 'Char' values, optimized for ASCII. The
-- set representation is unboxed for efficiency. For small sets, we
-- test for membership using a binary search. For larger sets, we use
-- a lookup table. Characters above the ASCII range (0x00-0x7f) are maintained 
-- in an IntSet, providing nominal O(1) lookup. 
--
-- Designed to be imported qualified:
-- 
-- > import Text.Trifecta.CharSet (CharSet)
-- > import qualified Text.Trifecta.CharSet as CharSet
--
-------------------------------------------------------------------------------

module Text.Trifecta.CharSet 
    ( 
    -- * Set type
      CharSet
    -- * Operators
    , (\\)
    -- * Query
    , null
    , size
    , member
    , notMember
    , overlaps, isSubsetOf
    , isComplemented 
    -- * Construction
    , build
    , empty
    , singleton
    , full
    , insert
    , delete
    , complement
    , range
    -- * Combine
    , union
    , intersection
    , difference
    -- * Filter
    , filter
    , partition
    -- * Map
    , map
    -- * Fold
    , fold
    -- * Conversion
    -- ** List
    , toList
    , fromList
    -- ** Ordered list
    , toAscList
    , fromAscList
    , fromDistinctAscList
    -- ** IntMaps
    , fromCharSet
    , toCharSet
    -- ** Array
    , toArray
    ) where

import Data.Array.Unboxed hiding (range)
import Data.Data
import Data.Function (on)
import Data.IntSet (IntSet)
import Text.Trifecta.CharSet.AsciiSet (AsciiSet)
import qualified Text.Trifecta.CharSet.AsciiSet as AsciiSet
import Data.Monoid (Monoid(..))
import qualified Data.IntSet as I
import qualified Data.List as L
import Prelude hiding (filter, map, null)
import qualified Prelude as P
import Text.Read

data CharSet 
  = Pos AsciiSet !IntSet 
  | Neg AsciiSet !IntSet

pos :: IntSet -> CharSet
pos s = Pos (AsciiSet.fromList (fmap toEnum (takeWhile (<= 0x7f) (I.toAscList s)))) s

neg :: IntSet -> CharSet
neg s = Neg (AsciiSet.fromList (fmap toEnum (takeWhile (<= 0x7f) (I.toAscList s)))) s

(\\) :: CharSet -> CharSet -> CharSet
(\\) = difference

build :: (Char -> Bool) -> CharSet
build p = fromDistinctAscList $ P.filter p [minBound .. maxBound]
{-# INLINE build #-}

map :: (Char -> Char) -> CharSet -> CharSet
map f (Pos _ i) = pos (I.map (fromEnum . f . toEnum) i)
map f (Neg _ i) = fromList $ P.map f $ P.filter (\x -> fromEnum x `I.notMember` i) [ul..uh] 
{-# INLINE map #-}

isComplemented :: CharSet -> Bool
isComplemented (Pos _ _) = False
isComplemented (Neg _ _) = True
{-# INLINE isComplemented #-}

toList :: CharSet -> String
toList (Pos _ i) = P.map toEnum (I.toList i)
toList (Neg _ i) = P.filter (\x -> fromEnum x `I.notMember` i) [ul..uh]
{-# INLINE toList #-}

toAscList :: CharSet -> String
toAscList (Pos _ i) = P.map toEnum (I.toAscList i)
toAscList (Neg _ i) = P.filter (\x -> fromEnum x `I.notMember` i) [ul..uh]
{-# INLINE toAscList #-}
    
empty :: CharSet
empty = pos I.empty

singleton :: Char -> CharSet
singleton = pos . I.singleton . fromEnum
{-# INLINE singleton #-}

full :: CharSet
full = neg I.empty

-- | /O(n)/ worst case
null :: CharSet -> Bool
null (Pos _ i) = I.null i
null (Neg _ i) = I.size i == numChars
{-# INLINE null #-}

-- | /O(n)/
size :: CharSet -> Int
size (Pos _ i) = I.size i
size (Neg _ i) = numChars - I.size i
{-# INLINE size #-}

insert :: Char -> CharSet -> CharSet
insert c (Pos _ i) = pos (I.insert (fromEnum c) i)
insert c (Neg _ i) = neg (I.delete (fromEnum c) i)
{-# INLINE insert #-}

range :: Char -> Char -> CharSet
range a b 
  | a <= b = fromDistinctAscList [a..b]
  | otherwise = empty

delete :: Char -> CharSet -> CharSet
delete c (Pos _ i) = pos (I.delete (fromEnum c) i)
delete c (Neg _ i) = neg (I.insert (fromEnum c) i)
{-# INLINE delete #-}

complement :: CharSet -> CharSet
complement (Pos s i) = Neg s i
complement (Neg s i) = Pos s i
{-# INLINE complement #-}

union :: CharSet -> CharSet -> CharSet
union (Pos _ i) (Pos _ j) = pos (I.union i j)
union (Pos _ i) (Neg _ j) = neg (I.difference j i)
union (Neg _ i) (Pos _ j) = neg (I.difference i j)
union (Neg _ i) (Neg _ j) = neg (I.intersection i j)
{-# INLINE union #-}

intersection :: CharSet -> CharSet -> CharSet
intersection (Pos _ i) (Pos _ j) = pos (I.intersection i j)
intersection (Pos _ i) (Neg _ j) = pos (I.difference i j)
intersection (Neg _ i) (Pos _ j) = pos (I.difference j i)
intersection (Neg _ i) (Neg _ j) = neg (I.union i j)
{-# INLINE intersection #-}

difference :: CharSet -> CharSet -> CharSet 
difference (Pos _ i) (Pos _ j) = pos (I.difference i j)
difference (Pos _ i) (Neg _ j) = pos (I.intersection i j)
difference (Neg _ i) (Pos _ j) = neg (I.union i j)
difference (Neg _ i) (Neg _ j) = pos (I.difference j i)
{-# INLINE difference #-}

member :: Char -> CharSet -> Bool
member c (Pos b i)
  | c <= toEnum 0x7f = AsciiSet.member c b
  | otherwise        = I.member (fromEnum c) i
member c (Neg b i) 
  | c <= toEnum 0x7f = not (AsciiSet.member c b)
  | otherwise        = I.notMember (fromEnum c) i
{-# INLINE member #-}

notMember :: Char -> CharSet -> Bool
notMember c s = not (member c s)
{-# INLINE notMember #-}

fold :: (Char -> b -> b) -> b -> CharSet -> b
fold f z (Pos _ i) = I.fold (f . toEnum) z i
fold f z (Neg _ i) = foldr f z $ P.filter (\x -> fromEnum x `I.notMember` i) [ul..uh]
{-# INLINE fold #-}

filter :: (Char -> Bool) -> CharSet -> CharSet 
filter p (Pos _ i) = pos (I.filter (p . toEnum) i)
filter p (Neg _ i) = neg $ foldr (I.insert) i $ P.filter (\x -> (x `I.notMember` i) && not (p (toEnum x))) [ol..oh]
{-# INLINE filter #-}

partition :: (Char -> Bool) -> CharSet -> (CharSet, CharSet)
partition p (Pos _ i) = (pos l, pos r)
    where (l,r) = I.partition (p . toEnum) i
partition p (Neg _ i) = (neg (foldr I.insert i l), neg (foldr I.insert i r))
    where (l,r) = L.partition (p . toEnum) $ P.filter (\x -> x `I.notMember` i) [ol..oh]
{-# INLINE partition #-}

overlaps :: CharSet -> CharSet -> Bool
overlaps (Pos _ i) (Pos _ j) = not (I.null (I.intersection i j))
overlaps (Pos _ i) (Neg _ j) = not (I.isSubsetOf j i)
overlaps (Neg _ i) (Pos _ j) = not (I.isSubsetOf i j)
overlaps (Neg _ i) (Neg _ j) = any (\x -> I.notMember x i && I.notMember x j) [ol..oh] -- not likely
{-# INLINE overlaps #-}

isSubsetOf :: CharSet -> CharSet -> Bool
isSubsetOf (Pos _ i) (Pos _ j) = I.isSubsetOf i j
isSubsetOf (Pos _ i) (Neg _ j) = I.null (I.intersection i j)
isSubsetOf (Neg _ i) (Pos _ j) = all (\x -> I.member x i && I.member x j) [ol..oh] -- not bloody likely
isSubsetOf (Neg _ i) (Neg _ j) = I.isSubsetOf j i
{-# INLINE isSubsetOf #-}

fromList :: String -> CharSet 
fromList = pos . I.fromList . P.map fromEnum
{-# INLINE fromList #-}

fromAscList :: String -> CharSet
fromAscList = pos . I.fromAscList . P.map fromEnum
{-# INLINE fromAscList #-}

fromDistinctAscList :: String -> CharSet
fromDistinctAscList = pos . I.fromDistinctAscList . P.map fromEnum
{-# INLINE fromDistinctAscList #-}

-- isProperSubsetOf :: CharSet -> CharSet -> Bool
-- isProperSubsetOf (P i) (P j) = I.isProperSubsetOf i j
-- isProperSubsetOf (P i) (N j) = null (I.intersection i j) && ...
-- isProperSubsetOf (N i) (N j) = I.isProperSubsetOf j i

ul, uh :: Char
ul = minBound
uh = maxBound
{-# INLINE ul #-}
{-# INLINE uh #-}

ol, oh :: Int
ol = fromEnum ul
oh = fromEnum uh
{-# INLINE ol #-}
{-# INLINE oh #-}

numChars :: Int
numChars = oh - ol + 1
{-# INLINE numChars #-}

instance Typeable CharSet where
  typeOf _ = mkTyConApp charSetTyCon []

charSetTyCon :: TyCon
charSetTyCon = mkTyCon "Text.Trifecta.CharSet.CharSet"
{-# NOINLINE charSetTyCon #-}

instance Data CharSet where
  gfoldl k z set 
    | isComplemented set = z complement `k` complement set
    | otherwise          = z fromList `k` toList set

  toConstr set 
    | isComplemented set = complementConstr
    | otherwise = fromListConstr

  dataTypeOf _ = charSetDataType

  gunfold k z c = case constrIndex c of
    1 -> k (z fromList)
    2 -> k (z complement)
    _ -> error "gunfold"

fromListConstr :: Constr
fromListConstr   = mkConstr charSetDataType "fromList" [] Prefix
{-# NOINLINE fromListConstr #-}

complementConstr :: Constr
complementConstr = mkConstr charSetDataType "complement" [] Prefix
{-# NOINLINE complementConstr #-}

charSetDataType :: DataType
charSetDataType  = mkDataType "Text.Trifecta.CharSet.CharSet" [fromListConstr, complementConstr]
{-# NOINLINE charSetDataType #-}

-- returns an intset and if the intset should be complemented to obtain the contents of the CharSet
fromCharSet :: CharSet -> (Bool, IntSet)
fromCharSet (Pos _ i) = (False, i)
fromCharSet (Neg _ i) = (True, i) 
{-# INLINE fromCharSet #-}

toCharSet :: IntSet -> CharSet
toCharSet = pos
{-# INLINE toCharSet #-}

instance Eq CharSet where
  (==) = (==) `on` toAscList

instance Ord CharSet where
  compare = compare `on` toAscList

instance Bounded CharSet where
  minBound = empty
  maxBound = full

-- TODO return a tighter bounded array perhaps starting from the least element present to the last element present?
toArray :: CharSet -> UArray Char Bool
toArray set = array (minBound, maxBound) $ fmap (\x -> (x, x `member` set)) [minBound .. maxBound]
 
instance Show CharSet where
  showsPrec d i
    | isComplemented i = showParen (d > 10) $ showString "complement " . showsPrec 11 (complement i)
    | otherwise        = showParen (d > 10) $ showString "fromDistinctAscList " . showsPrec 11 (toAscList i)

instance Read CharSet where
  readPrec = parens $ complemented +++ normal 
    where
      complemented = prec 10 $ do 
        Ident "complement" <- lexP
        complement `fmap` step readPrec
      normal = prec 10 $ do
        Ident "fromDistinctAscList" <- lexP
        fromDistinctAscList `fmap` step readPrec

instance Monoid CharSet where
  mempty = empty
  mappend = union

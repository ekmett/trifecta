{-# OPTIONS_GHC -fspec-constr #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Trifecta.CharSet
-- Copyright   :  (c) Edward Kmett 2010-2011
-- License     :  BSD3
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable (Data, BangPatterns, MagicHash)
--
-- Fast set membership tests for 'Char' values
--
-- Stored as a (possibly negated) IntMap and a fast set used for the head byte.
--
-- The set of valid (possibly negated) head bytes is stored unboxed as a 32-byte
-- bytestring-based lookup table.
--
-- Designed to be imported qualified:
-- 
-- > import Text.Trifecta.CharSet (CharSet)
-- > import qualified Text.Trifecta.CharSet as CharSet
--
 -------------------------------------------------------------------------------
--
-------------------------------------------------------------------------------

module Text.Trifecta.CharSet
    ( 
    -- * Set type
      CharSet(..)
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
import Text.Trifecta.ByteSet (ByteSet)
import qualified Text.Trifecta.ByteSet as ByteSet
import Data.Bits hiding (complement)
import Data.Word
import Data.ByteString.Internal (c2w)
import Data.Monoid (Monoid(..))
import qualified Data.IntSet as I
import qualified Data.List as L
import Prelude hiding (filter, map, null)
import qualified Prelude as P
import Text.Read

data CharSet = CharSet !Bool {-# UNPACK #-} !ByteSet !IntSet 

charSet :: Bool -> IntSet -> CharSet
charSet b s = CharSet b (ByteSet.fromList (fmap headByte (I.toAscList s))) s

headByte :: Int -> Word8
headByte i 
  | i <= 0x7f   = toEnum i 
  | i <= 0x7ff  = toEnum $ 0x80 + (i `shiftR` 6)
  | i <= 0xffff = toEnum $ 0xe0 + (i `shiftR` 12)
  | otherwise   = toEnum $ 0xf0 + (i `shiftR` 18)

pos :: IntSet -> CharSet
pos = charSet True

neg :: IntSet -> CharSet
neg = charSet False

(\\) :: CharSet -> CharSet -> CharSet
(\\) = difference

build :: (Char -> Bool) -> CharSet
build p = fromDistinctAscList $ P.filter p [minBound .. maxBound]
{-# INLINE build #-}

map :: (Char -> Char) -> CharSet -> CharSet
map f (CharSet True _ i) = pos (I.map (fromEnum . f . toEnum) i)
map f (CharSet False _ i) = fromList $ P.map f $ P.filter (\x -> fromEnum x `I.notMember` i) [ul..uh] 
{-# INLINE map #-}

isComplemented :: CharSet -> Bool
isComplemented (CharSet True _ _) = False
isComplemented (CharSet False _ _) = True
{-# INLINE isComplemented #-}

toList :: CharSet -> String
toList (CharSet True _ i) = P.map toEnum (I.toList i)
toList (CharSet False _ i) = P.filter (\x -> fromEnum x `I.notMember` i) [ul..uh]
{-# INLINE toList #-}

toAscList :: CharSet -> String
toAscList (CharSet True _ i) = P.map toEnum (I.toAscList i)
toAscList (CharSet False _ i) = P.filter (\x -> fromEnum x `I.notMember` i) [ul..uh]
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
null (CharSet True _ i) = I.null i
null (CharSet False _ i) = I.size i == numChars
{-# INLINE null #-}

-- | /O(n)/
size :: CharSet -> Int
size (CharSet True _ i) = I.size i
size (CharSet False _ i) = numChars - I.size i
{-# INLINE size #-}

insert :: Char -> CharSet -> CharSet
insert c (CharSet True _ i) = pos (I.insert (fromEnum c) i)
insert c (CharSet False _ i) = neg (I.delete (fromEnum c) i)
{-# INLINE insert #-}

range :: Char -> Char -> CharSet
range a b 
  | a <= b = fromDistinctAscList [a..b]
  | otherwise = empty

delete :: Char -> CharSet -> CharSet
delete c (CharSet True _ i) = pos (I.delete (fromEnum c) i)
delete c (CharSet False _ i) = neg (I.insert (fromEnum c) i)
{-# INLINE delete #-}

complement :: CharSet -> CharSet
complement (CharSet True s i) = CharSet False s i
complement (CharSet False s i) = CharSet True s i
{-# INLINE complement #-}

union :: CharSet -> CharSet -> CharSet
union (CharSet True _ i) (CharSet True _ j) = pos (I.union i j)
union (CharSet True _ i) (CharSet False _ j) = neg (I.difference j i)
union (CharSet False _ i) (CharSet True _ j) = neg (I.difference i j)
union (CharSet False _ i) (CharSet False _ j) = neg (I.intersection i j)
{-# INLINE union #-}

intersection :: CharSet -> CharSet -> CharSet
intersection (CharSet True _ i) (CharSet True _ j) = pos (I.intersection i j)
intersection (CharSet True _ i) (CharSet False _ j) = pos (I.difference i j)
intersection (CharSet False _ i) (CharSet True _ j) = pos (I.difference j i)
intersection (CharSet False _ i) (CharSet False _ j) = neg (I.union i j)
{-# INLINE intersection #-}

difference :: CharSet -> CharSet -> CharSet 
difference (CharSet True _ i) (CharSet True _ j) = pos (I.difference i j)
difference (CharSet True _ i) (CharSet False _ j) = pos (I.intersection i j)
difference (CharSet False _ i) (CharSet True _ j) = neg (I.union i j)
difference (CharSet False _ i) (CharSet False _ j) = pos (I.difference j i)
{-# INLINE difference #-}

member :: Char -> CharSet -> Bool
member c (CharSet True b i)
  | c <= toEnum 0x7f = ByteSet.member (c2w c) b
  | otherwise        = I.member (fromEnum c) i
member c (CharSet False b i) 
  | c <= toEnum 0x7f = not (ByteSet.member (c2w c) b)
  | otherwise        = I.notMember (fromEnum c) i
{-# INLINE member #-}

notMember :: Char -> CharSet -> Bool
notMember c s = not (member c s)
{-# INLINE notMember #-}

fold :: (Char -> b -> b) -> b -> CharSet -> b
fold f z (CharSet True _ i) = I.fold (f . toEnum) z i
fold f z (CharSet False _ i) = foldr f z $ P.filter (\x -> fromEnum x `I.notMember` i) [ul..uh]
{-# INLINE fold #-}

filter :: (Char -> Bool) -> CharSet -> CharSet 
filter p (CharSet True _ i) = pos (I.filter (p . toEnum) i)
filter p (CharSet False _ i) = neg $ foldr (I.insert) i $ P.filter (\x -> (x `I.notMember` i) && not (p (toEnum x))) [ol..oh]
{-# INLINE filter #-}

partition :: (Char -> Bool) -> CharSet -> (CharSet, CharSet)
partition p (CharSet True _ i) = (pos l, pos r)
    where (l,r) = I.partition (p . toEnum) i
partition p (CharSet False _ i) = (neg (foldr I.insert i l), neg (foldr I.insert i r))
    where (l,r) = L.partition (p . toEnum) $ P.filter (\x -> x `I.notMember` i) [ol..oh]
{-# INLINE partition #-}

overlaps :: CharSet -> CharSet -> Bool
overlaps (CharSet True _ i) (CharSet True _ j) = not (I.null (I.intersection i j))
overlaps (CharSet True _ i) (CharSet False _ j) = not (I.isSubsetOf j i)
overlaps (CharSet False _ i) (CharSet True _ j) = not (I.isSubsetOf i j)
overlaps (CharSet False _ i) (CharSet False _ j) = any (\x -> I.notMember x i && I.notMember x j) [ol..oh] -- not likely
{-# INLINE overlaps #-}

isSubsetOf :: CharSet -> CharSet -> Bool
isSubsetOf (CharSet True _ i) (CharSet True _ j) = I.isSubsetOf i j
isSubsetOf (CharSet True _ i) (CharSet False _ j) = I.null (I.intersection i j)
isSubsetOf (CharSet False _ i) (CharSet True _ j) = all (\x -> I.member x i && I.member x j) [ol..oh] -- not bloody likely
isSubsetOf (CharSet False _ i) (CharSet False _ j) = I.isSubsetOf j i
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

-- returns an intset and if the charSet is positive
fromCharSet :: CharSet -> (Bool, IntSet)
fromCharSet (CharSet b _ i) = (b, i)
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

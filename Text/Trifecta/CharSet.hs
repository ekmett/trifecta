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
-- Fast set membership tests for 'Char' values
--
-- Stored as a (possibly negated IntMap) and a fast set for the ASCII range.
--
-- The ASCII range is unboxed for efficiency. For small (complemented) sets, we
-- test for membership using a binary search. For larger (complemented) sets
-- we use a lookup table. 
--
-- Designed to be imported qualified:
-- 
-- > import Text.Trifecta.CharSet.Prim (CharSet)
-- > import qualified Text.Trifecta.CharSet.Prim as CharSet
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
    , module Text.Trifecta.CharSet.Common
    ) where

import Text.Trifecta.CharSet.Prim
import Text.Trifecta.CharSet.Common
import Prelude ()


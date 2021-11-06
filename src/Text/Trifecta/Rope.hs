{-# language BangPatterns          #-}
{-# language CPP                   #-}
{-# language DeriveDataTypeable    #-}
{-# language DeriveGeneric         #-}
{-# language FlexibleInstances     #-}
{-# language MultiParamTypeClasses #-}
{-# language TypeFamilies          #-}
-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2011-2019 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- A rope is a data strucure to efficiently store and manipulate long strings.
-- Wikipedia provides a nice overview:
-- <https://en.wikipedia.org/wiki/Rope_(data_structure)>
----------------------------------------------------------------------------
module Text.Trifecta.Rope
  ( Rope(..)
  , rope
  , ropeBS
  , Strand(..)
  , strand
  , strands
  , grabRest
  , grabLine
  ) where

import           Data.ByteString        (ByteString)
import qualified Data.ByteString        as Strict
import qualified Data.ByteString.Lazy   as Lazy
import qualified Data.ByteString.UTF8   as UTF8
import           Data.Data
import           Data.FingerTree        as FingerTree
import           Data.Foldable          (toList)
import           Data.Hashable
#if !(MIN_VERSION_base(4,11,0))
import           Data.Semigroup
#endif
import           Data.Semigroup.Reducer
import           GHC.Generics

import Text.Trifecta.Delta
import Text.Trifecta.Util.Combinators as Util

-- $setup
--
-- >>> :set -XOverloadedStrings
-- >>> import Data.Monoid ((<>))
-- >>> import Data.ByteString (ByteString)
-- >>> import qualified Data.ByteString.UTF8 as Strict
-- >>> import qualified Data.ByteString.Lazy.UTF8 as Lazy
-- >>> import Text.Trifecta.Delta

-- A 'Strand' is a chunk of data; many 'Strand's together make a 'Rope'.
data Strand
  = Strand {-# UNPACK #-} !ByteString !Delta -- ^ Data of a certain length
  | Skipping !Delta                          -- ^ Absence of data of a certain length
  deriving (Show, Data, Generic)

-- | Construct a single 'Strand' out of a 'ByteString'.
strand :: ByteString -> Strand
strand bs = Strand bs (delta bs)

instance Measured Delta Strand where
  measure (Strand _ s) = delta s
  measure (Skipping d) = d

instance Hashable Strand

instance HasDelta Strand where
  delta = measure

instance HasBytes Strand where
  bytes (Strand _ d) = bytes d
  bytes _            = 0

data Rope = Rope !Delta !(FingerTree Delta Strand) deriving Show

rope :: FingerTree Delta Strand -> Rope
rope r = Rope (measure r) r

-- | Construct a 'Rope' out of a single 'ByteString' strand.
ropeBS :: ByteString -> Rope
ropeBS = rope . singleton . strand

strands :: Rope -> FingerTree Delta Strand
strands (Rope _ r) = r

-- | Grab the entire rest of the input 'Rope', starting at an initial offset, or
-- return a default if we’re already at or beyond the end. Also see 'grabLine'.
--
-- Extract a suffix of a certain length from the input:
--
-- >>> grabRest (delta ("Hello " :: ByteString)) (ropeBS "Hello World\nLorem") Nothing (\x y -> Just (x, Lazy.toString y))
-- Just (Columns 6 6,"World\nLorem")
--
-- Same deal, but over multiple strands:
--
-- >>> grabRest (delta ("Hel" :: ByteString)) (ropeBS "Hello" <> ropeBS "World") Nothing (\x y -> Just (x, Lazy.toString y))
-- Just (Columns 3 3,"loWorld")
--
-- When the offset is too long, fall back to a default:
--
-- >>> grabRest (delta ("OffetTooLong" :: ByteString)) (ropeBS "Hello") Nothing (\x y -> Just (x, Lazy.toString y))
-- Nothing
grabRest
    :: Delta -- ^ Initial offset
    -> Rope  -- ^ Input
    -> r     -- ^ Default value if there is no input left
    -> (Delta -> Lazy.ByteString -> r)
        -- ^ If there is some input left, create an @r@ out of the data from the
        -- initial offset until the end
    -> r
grabRest offset input failure success = trim (delta l) (bytes offset - bytes l) (toList r) where
  trim offset' 0 (Strand str _ : xs) = go offset' str xs
  trim _       k (Strand str _ : xs) = go offset (Strict.drop (fromIntegral k) str) xs
  trim offset' k (Skipping p   : xs) = trim (offset' <> p) k xs
  trim _       _ []                  = failure

  go offset' str strands'
    = success offset' (Lazy.fromChunks (str : [ a | Strand a _ <- strands' ]))

  (l, r) = splitRopeAt offset input

-- | Split the rope in two halves, given a 'Delta' offset from the beginning.
splitRopeAt :: Delta -> Rope -> (FingerTree Delta Strand, FingerTree Delta Strand)
splitRopeAt splitPos = FingerTree.split (\pos -> bytes pos > bytes splitPos) . strands

-- | Grab the rest of the line at a certain offset in the input 'Rope', or
-- return a default if there is no newline left in the input. Also see
-- 'grabRest'.
--
-- >>> grabLine (delta ("Hello " :: ByteString)) (ropeBS "Hello" <> ropeBS " World\nLorem") Nothing (\x y -> Just (x, Strict.toString y))
-- Just (Columns 6 6,"World\n")
grabLine
    :: Delta -- ^ Initial offset
    -> Rope  -- ^ Input
    -> r     -- ^ Default value if there is no input left
    -> (Delta -> Strict.ByteString -> r)
        -- ^ If there is some input left, create an @r@ out of the data from the
        -- initial offset until the end of the line
    -> r
grabLine offset input failure success
  = grabRest offset input failure (\d -> success d . Util.fromLazy . Util.takeLine)

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

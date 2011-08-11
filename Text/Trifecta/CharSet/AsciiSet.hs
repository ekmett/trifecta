{-# LANGUAGE BangPatterns, MagicHash #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Trifecta.CharSet.AsciiSet
-- Copyright   :  Edward Kmett 2011
--                Bryan O'Sullivan 2008
-- License     :  BSD3
-- 
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Fast set membership tests for ASCII values, The
-- set representation is unboxed for efficiency. For small sets, we
-- test for membership using a binary search.  For larger sets, we use
-- a lookup table. 
-----------------------------------------------------------------------------
module Text.Trifecta.CharSet.AsciiSet
    (
    -- * Data type
      AsciiSet
    -- * Construction
    , fromList
    -- * Lookup
    , member
    ) where

import Data.Bits ((.&.), (.|.))
import Foreign.Storable (peekByteOff, pokeByteOff)
import GHC.Base (Int(I#), iShiftRA#, narrow8Word#, shiftL#)
import GHC.Word (Word8(W8#))
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Internal as I
import qualified Data.ByteString.Unsafe as U

data AsciiSet 
  = Sorted !B.ByteString
  | Table  !B.ByteString
  deriving (Eq, Ord, Show)

-- | The lower bound on the size of a lookup table.  We choose this to
-- balance table density against performance.
tableCutoff :: Int
tableCutoff = 8

-- | Create a set.
fromList :: [Char] -> AsciiSet
fromList s 
  | B.length bs < tableCutoff = Sorted (B.sort bs)
  | otherwise                 = Table (mkTable bs)
  where
    bs = B8.pack (filter (<= toEnum 0x7f) s)

-- | Check the set for membership.
member :: Char -> AsciiSet -> Bool
member c (Table t) = U.unsafeIndex t byte .&. bit /= 0
  where 
    i = fromEnum c
    I byte bit = index i
member c (Sorted s) = search 0 (B.length s - 1)
  where 
    w = I.c2w c
    search lo hi
      | hi < lo = False
      | otherwise = let mid = (lo + hi) `div` 2 in
                    case compare w (U.unsafeIndex s mid) of
        GT -> search (mid + 1) hi
        LT -> search lo (mid - 1)
        _ -> True

data I = I {-# UNPACK #-} !Int {-# UNPACK #-} !Word8

shiftR :: Int -> Int -> Int
shiftR (I# x#) (I# i#) = I# (x# `iShiftRA#` i#)

shiftL :: Word8 -> Int -> Word8
shiftL (W8# x#) (I# i#) = W8# (narrow8Word# (x# `shiftL#` i#))

index :: Int -> I
index i = I (i `shiftR` 3) (1 `shiftL` (i .&. 7))
{-# INLINE index #-}

mkTable :: B.ByteString -> B.ByteString
mkTable s = I.unsafeCreate 32 $ \t -> do
  _ <- I.memset t 0 32
  U.unsafeUseAsCStringLen s $ \(p, l) ->
    let loop n 
          | n == l = return ()
          | otherwise = do
            c <- peekByteOff p n :: IO Word8
            let I byte bit = index (fromIntegral c)
            prev <- peekByteOff t byte :: IO Word8
            pokeByteOff t byte (prev .|. bit)
            loop (n + 1)
    in loop 0

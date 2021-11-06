{-# language CPP #-}
{-# language DeriveDataTypeable #-}
{-# language DeriveGeneric #-}
-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2011-2019 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- A 'Delta' keeps track of the cursor position of the parser, so it can be
-- referred to later, for example in error messages.
----------------------------------------------------------------------------
module Text.Trifecta.Delta
  ( Delta(..)
  , HasDelta(..)
  , HasBytes(..)
  , prettyDelta
  , nextTab
  , rewind
  , near
  , column
  , columnByte
  ) where

#if !(MIN_VERSION_base(4,11,0))
import Data.Semigroup
#endif
import Data.Hashable
import Data.Int
import Data.Data
import Data.Word
import Data.Function (on)
import Data.FingerTree hiding (empty)
import Data.ByteString as Strict hiding (empty)
import qualified Data.ByteString.UTF8 as UTF8
import GHC.Generics
import Prettyprinter hiding (column, line')

import Text.Trifecta.Util.Pretty

class HasBytes t where
  bytes :: t -> Int64

instance HasBytes ByteString where
  bytes = fromIntegral . Strict.length

instance (Measured v a, HasBytes v) => HasBytes (FingerTree v a) where
  bytes = bytes . measure

-- | Since there are multiple ways to be at a certain location, 'Delta' captures
-- all these alternatives as a single type.
data Delta
  = Columns {-# UNPACK #-} !Int64
            {-# UNPACK #-} !Int64
    -- ^ @
    -- ( number of characters
    -- , number of bytes )
    -- @

  | Tab {-# UNPACK #-} !Int64
        {-# UNPACK #-} !Int64
        {-# UNPACK #-} !Int64
    -- ^ @
    -- ( number of characters before the tab
    -- , number of characters after the tab
    -- , number of bytes )
    -- @

  | Lines {-# UNPACK #-} !Int64
          {-# UNPACK #-} !Int64
          {-# UNPACK #-} !Int64
          {-# UNPACK #-} !Int64
    -- ^ @
    -- ( number of newlines contained
    -- , number of characters since the last newline
    -- , number of bytes
    -- , number of bytes since the last newline )
    -- @

  | Directed !ByteString
             {-# UNPACK #-} !Int64
             {-# UNPACK #-} !Int64
             {-# UNPACK #-} !Int64
             {-# UNPACK #-} !Int64
    -- ^ @
    -- ( current file name
    -- , number of lines since the last line directive
    -- , number of characters since the last newline
    -- , number of bytes
    -- , number of bytes since the last newline )
    -- @
  deriving (Show, Data, Generic)

instance Eq Delta where
  (==) = (==) `on` bytes

instance Ord Delta where
  compare = compare `on` bytes

instance (HasDelta l, HasDelta r) => HasDelta (Either l r) where
  delta = either delta delta

-- | Example: @file.txt:12:34@
prettyDelta :: Delta -> Doc AnsiStyle
prettyDelta d = case d of
    Columns c _         -> go interactive 0 c
    Tab x y _           -> go interactive 0 (nextTab x + y)
    Lines l c _ _       -> go interactive l c
    Directed fn l c _ _ -> go (UTF8.toString fn) l c
  where
    go
        :: String -- Source description
        -> Int64  -- Line
        -> Int64  -- Column
        -> Doc AnsiStyle
    go source line' column'
      = annotate bold (pretty source)
        <> char ':' <> annotate bold (pretty (line'+1))
        <> char ':' <> annotate bold (pretty (column'+1))
    interactive = "(interactive)"

-- | Retrieve the character offset within the current line from this 'Delta'.
column :: HasDelta t => t -> Int64
column t = case delta t of
  Columns c _ -> c
  Tab b a _ -> nextTab b + a
  Lines _ c _ _ -> c
  Directed _ _ c _ _ -> c
{-# inlinable column #-}

-- | Retrieve the byte offset within the current line from this 'Delta'.
columnByte :: Delta -> Int64
columnByte (Columns _ b) = b
columnByte (Tab _ _ b) = b
columnByte (Lines _ _ _ b) = b
columnByte (Directed _ _ _ _ b) = b
{-# inlinable columnByte #-}

instance HasBytes Delta where
  bytes (Columns _ b) = b
  bytes (Tab _ _ b) = b
  bytes (Lines _ _ b _) = b
  bytes (Directed _ _ _ b _) = b

instance Hashable Delta

instance Monoid Delta where
  mempty = Columns 0 0
  mappend = (<>)

instance Semigroup Delta where
  Columns c a        <> Columns d b         = Columns            (c + d)                            (a + b)
  Columns c a        <> Tab x y b           = Tab                (c + x) y                          (a + b)
  Columns _ a        <> Lines l c t a'      = Lines      l       c                         (t + a)  a'
  Columns _ a        <> Directed p l c t a' = Directed p l       c                         (t + a)  a'
  Lines l c t a      <> Columns d b         = Lines      l       (c + d)                   (t + b)  (a + b)
  Lines l c t a      <> Tab x y b           = Lines      l       (nextTab (c + x) + y)     (t + b)  (a + b)
  Lines l _ t _      <> Lines m d t' b      = Lines      (l + m) d                         (t + t') b
  Lines _ _ t _      <> Directed p l c t' a = Directed p l       c                         (t + t') a
  Tab x y a          <> Columns d b         = Tab                x (y + d)                          (a + b)
  Tab x y a          <> Tab x' y' b         = Tab                x (nextTab (y + x') + y')          (a + b)
  Tab _ _ a          <> Lines l c t a'      = Lines      l       c                         (t + a ) a'
  Tab _ _ a          <> Directed p l c t a' = Directed p l       c                         (t + a ) a'
  Directed p l c t a <> Columns d b         = Directed p l       (c + d)                   (t + b ) (a + b)
  Directed p l c t a <> Tab x y b           = Directed p l       (nextTab (c + x) + y)     (t + b ) (a + b)
  Directed p l _ t _ <> Lines m d t' b      = Directed p (l + m) d                         (t + t') b
  Directed _ _ _ t _ <> Directed p l c t' b = Directed p l       c                         (t + t') b

-- | Increment a column number to the next tabstop.
nextTab :: Int64 -> Int64
nextTab x = x + (8 - mod x 8)
{-# inlinable nextTab #-}

-- | Rewind a 'Delta' to the beginning of the line.
rewind :: Delta -> Delta
rewind (Lines n _ b d)      = Lines n 0 (b - d) 0
rewind (Directed p n _ b d) = Directed p n 0 (b - d) 0
rewind _                    = Columns 0 0
{-# inlinable rewind #-}

-- | Should we show two things with a 'Delta' on the same line?
--
-- >>> near (Columns 0 0) (Columns 5 5)
-- True
--
-- >>> near (Lines 1 0 1 0) (Lines 2 4 4 2)
-- False
near :: (HasDelta s, HasDelta t) => s -> t -> Bool
near s t = rewind (delta s) == rewind (delta t)
{-# inlinable near #-}

class HasDelta t where
  delta :: t -> Delta

instance HasDelta Delta where
  delta = id

instance HasDelta Char where
  delta '\t' = Tab 0 0 1
  delta '\n' = Lines 1 0 1 0
  delta c
    | o <= 0x7f   = Columns 1 1
    | o <= 0x7ff  = Columns 1 2
    | o <= 0xffff = Columns 1 3
    | otherwise   = Columns 1 4
    where o = fromEnum c

instance HasDelta Word8 where
  delta 9  = Tab 0 0 1
  delta 10 = Lines 1 0 1 0
  delta n
    | n <= 0x7f              = Columns 1 1
    | n >= 0xc0 && n <= 0xf4 = Columns 1 1
    | otherwise              = Columns 0 1

instance HasDelta ByteString where
  delta = foldMap delta . unpack

instance (Measured v a, HasDelta v) => HasDelta (FingerTree v a) where
  delta = delta . measure

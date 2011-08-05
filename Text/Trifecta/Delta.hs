module Text.Trifecta.Delta 
  ( Delta(..) 
  , HasDelta(..)
  , nextTab
  ) where

import Data.Monoid
import Data.Semigroup
import Data.Hashable
import Data.Word
import Data.Foldable
import Data.ByteString
import Text.Trifecta.Path

data Delta
  = Directed                 !Path -- ^ the sequence of #line directives since the start of the file
              {-# UNPACK #-} !Int  -- ^ the number of lines since the last line directive
              {-# UNPACK #-} !Int  -- ^ the number of characters since the last newline
              {-# UNPACK #-} !Int  -- ^ the number of bytes since the last newline
  | Lines     {-# UNPACK #-} !Int  -- ^ the number of newlines contained
              {-# UNPACK #-} !Int  -- ^ the number of characters since the last newline
              {-# UNPACK #-} !Int  -- ^ the number of bytes since the last newline
  | Tab       {-# UNPACK #-} !Int  -- ^ the number of characters before the tab
              {-# UNPACK #-} !Int  -- ^ the number of characters after the tab
              {-# UNPACK #-} !Int  -- ^ the number of bytes in this range
  | Columns   {-# UNPACK #-} !Int  -- ^ the number of characters
              {-# UNPACK #-} !Int  -- ^ the number of bytes
  deriving Show

instance Hashable Delta where
  hash (Columns c a)      = 0 `hashWithSalt` c `hashWithSalt` a
  hash (Tab x y a)        = 1 `hashWithSalt` x `hashWithSalt` y `hashWithSalt` a
  hash (Lines l c a)      = 2 `hashWithSalt` l `hashWithSalt` c `hashWithSalt` a
  hash (Directed p l c a) = 3 `hashWithSalt` p `hashWithSalt` l `hashWithSalt` c `hashWithSalt` a

instance Monoid Delta where
  mempty = Columns 0 0
  mappend = (<>)

instance Semigroup Delta where
  Columns c a      <> Columns d b         = Columns (c + d) (a + b)
  Columns c a      <> Tab x y b           = Tab (c + x) y (a + b)
  Lines l c a      <> Columns d b         = Lines l (c + d) (a + b)
  Lines l _ _      <> Lines m d b         = Lines (l + m) d b
  Lines l c a      <> Tab x y b           = Lines l (nextTab (c + x) + y) (a + b)
  Tab x y a        <> Columns d b         = Tab x (y + d) (a + b)
  Tab x y a        <> Tab x' y' b         = Tab x (nextTab (y + x') + y') (a + b) 
  Directed p l _ a <> Lines m d b         = Directed p (l + m) d (a + b)
  Directed p l c a <> Columns d b         = Directed p l (c + d) (a + b)
  Directed p l c a <> Tab x y b           = Directed p l (nextTab (c + x) + y) (a + b)
  Directed p l _ _ <> Directed p' l' c' b = Directed (appendPath p l p') l' c' b
  _                <> t                   = t
  
nextTab :: Int -> Int
nextTab x = x + (8 - mod x 8)

class HasDelta t where
  delta :: t -> Delta

instance HasDelta Char where
  delta '\t' = Tab 0 0 1
  delta '\n' = Lines 1 0 0
  delta _    = Columns 1 1

instance HasDelta Word8 where
  delta 9  = Tab 0 0 1
  delta 10 = Lines 1 0 0
  delta n | n <= 0x7f              = Columns 1 1
          | n >= 0xc0 && n <= 0xf4 = Columns 1 1
          | otherwise              = Columns 0 1

instance HasDelta ByteString where
  delta = foldMap delta . unpack

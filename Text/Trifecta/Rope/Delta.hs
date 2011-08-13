module Text.Trifecta.Rope.Delta 
  ( Delta(..) 
  , HasDelta(..)
  , nextTab
  , rewind
  , near
  , column
  , columnByte
  ) where

import Control.Applicative
import Data.Monoid
import Data.Semigroup
import Data.Hashable
import Data.Word
import Data.Interned
import Data.Foldable
import Data.FingerTree hiding (empty)
import Data.ByteString hiding (empty)
import Text.Trifecta.Rope.Path
import Text.Trifecta.Rope.Bytes
import Text.PrettyPrint.Free hiding (column)
import System.Console.Terminfo.PrettyPrint

data Delta
  = Columns   {-# UNPACK #-} !Int  -- the number of characters
              {-# UNPACK #-} !Int  -- the number of bytes
  | Tab       {-# UNPACK #-} !Int  -- the number of characters before the tab
              {-# UNPACK #-} !Int  -- the number of characters after the tab
              {-# UNPACK #-} !Int  -- the number of bytes
  | Lines     {-# UNPACK #-} !Int  -- the number of newlines contained
              {-# UNPACK #-} !Int  -- the number of characters since the last newline
              {-# UNPACK #-} !Int  -- number of bytes
              {-# UNPACK #-} !Int  -- the number of bytes since the last newline
  | Directed                 !Path -- the sequence of #line directives since the start of the file
              {-# UNPACK #-} !Int  -- the number of lines since the last line directive
              {-# UNPACK #-} !Int  -- the number of characters since the last newline
              {-# UNPACK #-} !Int  -- number of bytes
              {-# UNPACK #-} !Int  -- the number of bytes since the last newline
  deriving (Eq, Ord, Show)

columnByte :: Delta -> Int
columnByte (Columns _ b) = b
columnByte (Tab _ _ b) = b
columnByte (Lines _ _ _ b) = b
columnByte (Directed _ _ _ _ b) = b

instance (HasDelta l, HasDelta r) => HasDelta (Either l r) where
  delta = either delta delta

instance Pretty Delta where
  pretty p = prettyTerm p *> empty

instance PrettyTerm Delta where
  prettyTerm d = case d of
    Columns c _ -> k f 0 c
    Tab x y _ -> k f 0 (nextTab x + y)
    Lines l c _ _ -> k f l c
    Directed (Path _ _ _ fn _ _) l c _ _ -> k (maybeFileName f unintern fn) l c  -- TODO: add include path
    where 
      k fn ln cn = bold (string fn)           
                <> char ':' 
                <> bold (int (ln + 1))         
                <> char ':' 
                <> bold (int (cn + 1))
      f = "(interactive)"

column :: HasDelta t => t -> Int
column t = case delta t of 
  Columns c _ -> c
  Tab b a _ -> nextTab b + a
  Lines _ c _ _ -> c
  Directed _ _ c _ _ -> c

instance HasBytes Delta where
  bytes (Columns _ b) = b
  bytes (Tab _ _ b) = b
  bytes (Lines _ _ b _) = b
  bytes (Directed _ _ _ b _) = b

instance Hashable Delta where
  hash (Columns c a)        = 0 `hashWithSalt` c `hashWithSalt` a
  hash (Tab x y a)          = 1 `hashWithSalt` x `hashWithSalt` y `hashWithSalt` a
  hash (Lines l c b a)      = 2 `hashWithSalt` l `hashWithSalt` c `hashWithSalt` b `hashWithSalt` a
  hash (Directed p l c b a) = 3 `hashWithSalt` p `hashWithSalt` l `hashWithSalt` c `hashWithSalt` b `hashWithSalt` a

instance Monoid Delta where
  mempty = Columns 0 0
  mappend = (<>)

instance Semigroup Delta where
  Columns c a        <> Columns d b            = Columns (c + d) (a + b)
  Columns c a        <> Tab x y b              = Tab (c + x) y (a + b)
  Columns _ a        <> Lines l c t a'         = Lines l c (t + a) a'
  Columns _ a        <> Directed p l c t a'    = Directed p l c (t + a) a'
  Lines l c t a      <> Columns d b            = Lines l (c + d) (t + b) (a + b)
  Lines l c t a      <> Tab x y b              = Lines l (nextTab (c + x) + y) (t + b) (a + b)
  Lines l _ t _      <> Lines m d t' b         = Lines (l + m) d (t + t') b
  Lines _ _ t _      <> Directed p l c t' a    = Directed p l c (t + t') a
  Tab x y a          <> Columns d b            = Tab x (y + d) (a + b)
  Tab x y a          <> Tab x' y' b            = Tab x (nextTab (y + x') + y') (a + b) 
  Tab _ _ a          <> Lines l c t a'         = Lines l c (t + a) a'
  Tab _ _ a          <> Directed p l c t a'    = Directed p l c (t + a) a' 
  Directed p l c t a <> Columns d b            = Directed p l (c + d) (t + b) (a + b)
  Directed p l c t a <> Tab x y b              = Directed p l (nextTab (c + x) + y) (t + b) (a + b)
  Directed p l _ t a <> Lines m d t' b         = Directed p (l + m) d (t + t') (a + b)
  Directed p l _ t _ <> Directed p' l' c' t' b = Directed (appendPath p l p') l' c' (t + t') b
  
nextTab :: Int -> Int
nextTab x = x + (8 - mod x 8)

rewind :: Delta -> Delta
rewind (Lines n _ b d)      = Lines n 0 (b - d) 0
rewind (Directed p n _ b d) = Directed p n 0 (b - d) 0
rewind _                    = Columns 0 0 

near :: (HasDelta s, HasDelta t) => s -> t -> Bool
near s t = case (delta s, delta t) of
  (Directed p l _ _ _, Directed p' l' _ _ _) ->  p == p' && l == l'
  (Lines l _ _ _, Lines l' _ _ _) ->             l == l'
  (Columns _ _, Columns _ _) -> True
  (Columns _ _, Tab _ _ _) -> True
  (Tab _ _ _, Columns _ _) -> True
  (Tab _ _ _, Tab _ _ _) -> True
  _ -> False

class HasDelta t where
  delta :: t -> Delta

instance HasDelta Delta where
  delta = id

instance HasDelta Char where
  delta '\t' = Tab 0 0 1
  delta '\n' = Lines 1 0 1 0
  delta c | o <= 0x7f   = Columns 1 1
          | o <= 0x7ff  = Columns 1 2
          | o <= 0xffff = Columns 1 3
          | otherwise   = Columns 1 4
    where o = fromEnum c

instance HasDelta Word8 where
  delta 9  = Tab 0 0 1
  delta 10 = Lines 1 0 1 0
  delta n | n <= 0x7f              = Columns 1 1
          | n >= 0xc0 && n <= 0xf4 = Columns 1 1
          | otherwise              = Columns 0 1

instance HasDelta ByteString where
  delta = foldMap delta . unpack

instance HasDelta Path where
  delta p = Directed p 0 0 0 0

instance (Measured v a, HasDelta v) => HasDelta (FingerTree v a) where
  delta = delta . measure

module Text.Trifecta.Cursor 
  ( Cursor(..)
  ) where

import Data.Hashable
import Data.Monoid
import Data.Sequence
import Data.Interned
import Data.Semigroup
import Data.Foldable

import Text.Trifecta.Delta

data Cursor = Cursor 
  { cursorBytes   :: {-# UNPACK #-} !Int
  , cursorDelta   :: !Delta
  , cursorSeqHash :: {-# UNPACK #-} !Int
  , cursorSeq     :: !(Seq Id)
  }

instance Eq Cursor where
  Cursor _ _ h k == Cursor _ _ h' k' = h == h' && k == k'

instance Hashable Cursor where
  hash (Cursor _ _ _ s) = hash (toList s)
    
instance Monoid Cursor where
  mempty = Cursor 0 mempty 0 mempty
  Cursor s d l q `mappend` Cursor s' d' l' q' = Cursor (s + s') (d <> d') (l + l') (q <> q')

instance Semigroup Cursor where
  Cursor s d l q <> Cursor s' d' l' q' = Cursor (s + s') (d <> d') (l + l') (q <> q')


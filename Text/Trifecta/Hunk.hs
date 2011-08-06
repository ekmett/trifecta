{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleInstances #-}
module Text.Trifecta.Hunk 
  ( Hunk(..)
  , hunk
  ) where

import Data.ByteString
import Data.FingerTree as FingerTree
import Data.Function (on)
import Data.Hashable
import Data.Interned
import Data.Text as Text
import Data.Text.ICU.Convert
import GHC.IO
import Text.Trifecta.Delta
import Text.Trifecta.Sid
import Text.PrettyPrint.Leijen.Extras

data Hunk = Hunk {-# UNPACK #-} !Id !Delta {-# UNPACK #-} !ByteString

instance HasSid Hunk where
  sid (Hunk h _ _) = 42 + h * 2

instance HasSids Hunk where
  sids s = FingerTree.singleton $! sid s

hunk :: ByteString -> Hunk
hunk = intern 

-- assuming utf8 encoding
prettyByteString :: ByteString -> Doc e
prettyByteString bs = string (Text.unpack (toUnicode (unsafeDupablePerformIO (open "UTF8" Nothing)) bs))

instance Pretty Hunk where
  pretty (Hunk _ _ bs) = prettyByteString bs

instance Show Hunk where
  showsPrec _ h = displayS (renderPretty 0.9 80 (pretty h))

instance Eq Hunk where
  (==) = (==) `on` identity

instance Hashable Hunk where
  hash = hash . identity

instance Ord Hunk where
  compare = compare `on` identity

instance HasDelta Hunk where
  delta (Hunk _ d _) = d

instance Interned Hunk where
  type Uninterned Hunk = ByteString
  data Description Hunk = Describe {-# UNPACK #-} !ByteString deriving (Eq)
  describe = Describe
  identify i bs = Hunk i (delta bs) bs
  identity (Hunk i _ _) = i
  cache = hunkCache

instance Uninternable Hunk where
  unintern (Hunk _ _ bs) = bs

instance Hashable (Description Hunk) where
  hash (Describe bs) = hash bs

hunkCache :: Cache Hunk
hunkCache = mkCache 
{-# NOINLINE hunkCache #-}

instance Measured Delta Hunk where
  measure = delta

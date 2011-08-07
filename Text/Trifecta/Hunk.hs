{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleInstances #-}
module Text.Trifecta.Hunk 
  ( Hunk(..)
  , hunk
  ) where

import Data.ByteString
import qualified Data.ByteString.UTF8 as UTF8
import Data.FingerTree as FingerTree
import Data.Function (on)
import Data.Hashable
import Data.Interned
import Data.String
import Text.Trifecta.Delta

data Hunk = Hunk {-# UNPACK #-} !Id !Delta {-# UNPACK #-} !ByteString
  deriving Show

hunk :: ByteString -> Hunk
hunk = intern 

-- instance Show Hunk where showsPrec d (Hunk _ _ b) = showsPrec d b

instance IsString Hunk where
  fromString = hunk . UTF8.fromString

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

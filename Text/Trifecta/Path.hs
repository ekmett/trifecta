{-# LANGUAGE TypeFamilies, FlexibleInstances, BangPatterns #-}
module Text.Trifecta.Path
  ( FileName
  , Path(..), History(..)
  , file
  , snocPath
  , path
  , appendPath
  , comparablePath
  -- * Internals
  , MaybeFileName(..)
  , maybeFileName
  ) where

import Data.Hashable
import Data.Interned
import Data.Interned.String
import Data.Function (on)
import Data.Semigroup
import Data.IntSet as IntSet

type FileName = InternedString

data Path = Path {-# UNPACK #-} !Id !IntSet !History !MaybeFileName {-# UNPACK #-} !Int [Int]
  deriving Show

pathIds :: Path -> IntSet
pathIds (Path _ is _ _ _ _) = is

comparablePath :: Path -> Path -> Bool
comparablePath x y = identity x `IntSet.member` pathIds y
                  || identity y `IntSet.member` pathIds x

instance Eq Path where
  (==) = (==) `on` identity

instance Ord Path where
  compare = compare `on` identity

data History = Continue !Path {-# UNPACK #-} !Int | Complete deriving (Eq, Show)

data MaybeFileName = JustFileName !FileName | NothingFileName deriving (Eq, Show)

maybeFileName :: r -> (FileName -> r) -> MaybeFileName -> r
maybeFileName n _ NothingFileName  = n
maybeFileName _ f (JustFileName a) = f a

file :: String -> Path 
file !n = path Complete (JustFileName (intern n)) 0 []

snocPath :: Path -> Int -> MaybeFileName -> Int -> [Int] -> Path
snocPath d l jf l' flags = path (Continue d l) jf l' flags

path :: History -> MaybeFileName -> Int -> [Int] -> Path
path !h !mf l flags = intern (UPath h mf l flags)

appendPath :: Path -> Int -> Path -> Path
appendPath p dl (Path _ _ Complete          mf l flags) = snocPath p dl mf l flags
appendPath p dl (Path _ _ (Continue p' dl') mf l flags) = snocPath (appendPath p dl p') dl' mf l flags

instance Semigroup Path where
  p <> p' = appendPath p 0 p'

data UninternedPath = UPath !History !MaybeFileName {-# UNPACK #-} !Int [Int]
data DHistory = DContinue {-# UNPACK #-} !Id {-# UNPACK #-} !Int | DComplete deriving Eq

instance Hashable DHistory where
  hash (DContinue x y) = y `hashWithSalt` x
  hash DComplete       = 0

instance Hashable Path where
  hash = hash . identity

instance Interned Path where
  type Uninterned Path = UninternedPath
  data Description Path = DPath !(Maybe Id) {-# UNPACK #-} !Int [Int] !DHistory deriving Eq
  describe (UPath h mf l flags) = DPath mi l flags $ case h of
    Continue p dl -> DContinue (identity p) dl
    Complete      -> DComplete 
    where 
      mi = case mf of 
        JustFileName f -> Just (identity f)
        NothingFileName -> Nothing
                     
  identify i (UPath h mf l flags) = Path i m h mf l flags 
    where 
      m = case h of 
        Complete -> IntSet.singleton i
        Continue (Path _ m' _ _ _ _) _ -> IntSet.insert i m'
                 
  identity (Path i _ _ _ _ _) = i
  cache = pathCache

instance Uninternable Path where
  unintern (Path _ _ h mf l flags) = UPath h mf l flags

instance Hashable (Description Path) where
  hash (DPath mi l flags dh) = l `hashWithSalt` mi `hashWithSalt` flags `hashWithSalt` dh

pathCache :: Cache Path
pathCache = mkCache
{-# NOINLINE pathCache #-}


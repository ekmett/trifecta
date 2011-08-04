{-# LANGUAGE TypeFamilies, FlexibleInstances, BangPatterns #-}
module Text.Trifecta.Path
  ( FileName
  , Path(..), History(..)
  , startPath
  , snocPath
  , path
  , appendPath
  , prettyPathWith
  ) where

import Data.Hashable
import Data.Interned
import Data.Interned.String
import Text.PrettyPrint.Leijen.Extras

type FileName = InternedString

data Path = Path {-# UNPACK #-} !Id !History !MaybeFileName {-# UNPACK #-} !Int [Int]

prettyPathWith :: (Doc e -> Doc e) -> Path -> Int -> Doc e 
prettyPathWith wrapDir = go where
  go (Path _ h mf l flags) delta 
     = addHistory 
     $ wrapDir $ hsep $ text "#" : pretty (l + delta) : addFile (map pretty flags) where
    addHistory = case h of
      More p d -> above (prettyPathWith wrapDir p d)
      Done -> id
    addFile = case mf of
      JustFileName f -> (:) (dquotes (pretty (unintern f)))
      NothingFileName -> id

instance Pretty Path where
  pretty p = prettyPathWith id p 0

instance Show Path where
  showsPrec _ p = displayS (renderPretty 0.9 80 (pretty p))

data History = More !Path {-# UNPACK #-} !Int | Done
data MaybeFileName = JustFileName !FileName | NothingFileName deriving Eq

startPath :: FileName -> Path 
startPath !n = path Done (JustFileName n) 0 []

snocPath :: Path -> Int -> MaybeFileName -> Int -> [Int] -> Path
snocPath d l jf l' flags = path (More d l) jf l' flags

-- does case analysis to ensure the Maybe carries a fully evaluated argument
path :: History -> MaybeFileName -> Int -> [Int] -> Path
path !h !mf l flags = intern (UPath h mf l flags)

appendPath :: Path -> Int -> Path -> Path
appendPath p dl (Path _ Done          mf l flags) = snocPath p dl mf l flags
appendPath p dl (Path _ (More p' dl') mf l flags) = snocPath (appendPath p dl p') dl' mf l flags

data UninternedPath = UPath !History !MaybeFileName {-# UNPACK #-} !Int [Int]
data DHistory = DMore {-# UNPACK #-} !Id {-# UNPACK #-} !Int | DDone deriving Eq

instance Hashable DHistory where
  hash (DMore x y) = y `hashWithSalt` x
  hash DDone       = 0

instance Hashable Path where
  hash = hash . identity

instance Interned Path where
  type Uninterned Path = UninternedPath
  data Description Path = DPath !(Maybe Id) {-# UNPACK #-} !Int [Int] !DHistory deriving Eq
  describe (UPath h mf l flags) = DPath mi l flags $ case h of
    More p dl -> DMore (identity p) dl
    Done -> DDone 
    where 
      mi = case mf of 
        JustFileName f -> Just (identity f)
        NothingFileName -> Nothing
                     
  identify i (UPath h mf l flags) = Path i h mf l flags
  identity (Path i _ _ _ _) = i
  cache = pathCache

instance Uninternable Path where
  unintern (Path _ h mf l flags) = UPath h mf l flags

instance Hashable (Description Path) where
  hash (DPath mi l flags dh) = l `hashWithSalt` mi `hashWithSalt` flags `hashWithSalt` dh

pathCache :: Cache Path
pathCache = mkCache
{-# NOINLINE pathCache #-}

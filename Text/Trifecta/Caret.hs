{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FlexibleInstances, BangPatterns, PatternGuards #-}
module Text.Trifecta.Caret
  ( Caret(..)
  , HasCaret(..)
  , Careted(..)
  , Cover(..)
  , HasCover(..)
  , Covered(..)
  , Fixit(..)
  , Diagnostic(..)
  ) where

import Data.Hashable
import Data.ByteString as Strict
import Text.Trifecta.Delta

-- |
-- > In file included from baz.c:9
-- > In file included from bar.c:4
-- > foo.c:8:36: note
-- > int main(int argc, char ** argv) { int; }
-- >                                    ^
data Caret = Caret !Delta {-# UNPACK #-} !ByteString deriving (Eq,Ord,Show)

instance Hashable Caret where
  hash (Caret d bs) = hash d `hashWithSalt` bs

class HasCaret t where
  caret :: t -> Caret

instance HasCaret Caret where
  caret = id

data Careted a = a :^ Caret deriving (Eq,Ord,Show)

instance HasCaret (Careted a) where
  caret (_ :^ c) = c

instance Hashable a => Hashable (Careted a) where
  
-- |
-- > In file included from bar.c:9
-- > foo.c:8:36: note
-- > int main(int argc, char ** argv) { int; }
-- >                                    ^~~
data Cover = Cover {-# UNPACK #-} !Caret !Delta deriving (Eq,Ord,Show)

instance HasCaret Cover where
  caret (Cover c _) = c

class HasCover t where
  cover :: t -> Cover

instance HasCover Cover where
  cover = id

data Covered a = a :~ Cover deriving (Eq,Ord,Show)

instance HasCover (Covered a) where
  cover (_ :~ c) = c

instance HasCaret (Covered a) where
  caret = caret . cover

instance Hashable Cover where
  hash (Cover c bs) = hash c `hashWithSalt` bs

instance Hashable a => Hashable (Covered a) where
  hash (a :~ s) = hash a `hashWithSalt` s

-- |
-- > In file included from bar.c:12
-- > foo.c:12:17: note
-- > int main(int argc char ** argv) { int; }
-- >                  ^
-- >                  ,
data Fixit = Fixit 
  { fixitCover        :: {-# UNPACK #-} !Cover
  , fixitReplacement  :: {-# UNPACK #-} !ByteString 
  } deriving (Eq,Ord,Show)

instance HasCover Fixit where
  cover (Fixit s _) = s

instance HasCaret Fixit where
  caret = caret . cover

instance Hashable Fixit where
  hash (Fixit s b) = hash s `hashWithSalt` b

-- |
-- > In file included from bar.h:12
-- > baz.h:12:17: note
-- > foo + bar
-- > ~~~ ^ ~~~
data Diagnostic = Diagnostic !Caret [Cover] [Fixit]

instance HasCaret Diagnostic where
  caret (Diagnostic c _ _) = c

instance Hashable Diagnostic where
  hash (Diagnostic c ss fs) = hash c `hashWithSalt` ss `hashWithSalt` fs

{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FlexibleInstances, BangPatterns, PatternGuards #-}
module Text.Trifecta.Caret
  ( Caret(..)
  , HasCaret(..)
  , Careted(..)
  , Span(..)
  , HasSpan(..)
  , Spanned(..)
  , Fixit(..)
  ) where

import Data.Hashable
import Data.ByteString (ByteString)
import qualified Data.ByteString.UTF8 as UTF8
import Text.Trifecta.Delta
import Text.Trifecta.Render
import Prelude hiding (span)

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

instance Renderable Caret where
  rendering (Caret d bs) = addCaret d $ surface d bs

data Careted a = a :^ Caret deriving (Eq,Ord,Show)

instance Renderable (Careted a) where
  rendering = rendering . caret

instance HasCaret (Careted a) where
  caret (_ :^ c) = c

instance Hashable a => Hashable (Careted a) where
  
-- |
-- > In file included from bar.c:9
-- > foo.c:8:36: note
-- > int main(int argc, char ** argv) { int; }
-- >                                    ^~~
data Span = Span !Delta !Delta {-# UNPACK #-} !ByteString deriving (Eq,Ord,Show)

instance HasCaret Span where
  caret (Span s _ b) = Caret s b

instance Renderable Span where
  rendering (Span s e bs) = addSpan s e $ surface s bs

class HasSpan t where
  span :: t -> Span

instance HasSpan Span where
  span = id

data Spanned a = a :~ Span deriving (Eq,Ord,Show)


instance HasSpan (Spanned a) where
  span (_ :~ c) = c

instance Renderable (Spanned a) where
  rendering = rendering . span

instance HasCaret (Spanned a) where
  caret = caret . span

instance Hashable Span where
  hash (Span s e bs) = hash s `hashWithSalt` e `hashWithSalt` bs

instance Hashable a => Hashable (Spanned a) where
  hash (a :~ s) = hash a `hashWithSalt` s

-- |
-- > In file included from bar.c:12
-- > foo.c:12:17: note
-- > int main(int argc char ** argv) { int; }
-- >                  ^
-- >                  ,
data Fixit = Fixit 
  { fixitSpan        :: {-# UNPACK #-} !Span
  , fixitReplacement  :: {-# UNPACK #-} !ByteString 
  } deriving (Eq,Ord,Show)

instance HasSpan Fixit where
  span (Fixit s _) = s

instance HasCaret Fixit where
  caret = caret . span

instance Hashable Fixit where
  hash (Fixit s b) = hash s `hashWithSalt` b

instance Renderable Fixit where
  rendering (Fixit (Span s e bs) r) = addFixit s e (UTF8.toString r) $ surface s bs

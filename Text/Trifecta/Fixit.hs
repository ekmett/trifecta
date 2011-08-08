module Text.Trifecta.Fixit
  ( Fixit(..)
  ) where

import Data.Hashable
import Data.ByteString (ByteString)
import qualified Data.ByteString.UTF8 as UTF8
import Text.Trifecta.Delta
import Text.Trifecta.Render
import Text.Trifecta.Caret
import Text.Trifecta.Span
import Prelude hiding (span)

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

instance HasDelta Fixit where
  delta = delta . caret

instance HasCaret Fixit where
  caret = caret . span

instance Hashable Fixit where
  hash (Fixit s b) = hash s `hashWithSalt` b

instance Renderable Fixit where
  render (Fixit (Span s e bs) r) = addFixit s e (UTF8.toString r) $ surface s bs

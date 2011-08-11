{-# LANGUAGE MultiParamTypeClasses #-}
module Text.Trifecta.Diagnostic.Rendering.Fixit
  ( Fixit(..)
  , drawFixit
  , addFixit
  , fixit
  ) where

import Data.Functor
import Data.Hashable
import Data.ByteString (ByteString)
import qualified Data.ByteString as Strict
import qualified Data.ByteString.UTF8 as UTF8
import Data.Semigroup.Reducer
import Text.Trifecta.Rope.Bytes
import Text.Trifecta.Rope.Delta
import Text.Trifecta.Diagnostic.Rendering.Prim
import Text.Trifecta.Diagnostic.Rendering.Span
import Text.Trifecta.Parser.Class
import Text.Trifecta.Util
import System.Console.Terminfo.Color
import System.Console.Terminfo.PrettyPrint
import Prelude hiding (span)

-- |
-- > int main(int argc char ** argv) { int; }
-- >                  ^
-- >                  ,
drawFixit :: Delta -> Delta -> String -> Delta -> Lines -> Lines
drawFixit s e rpl d a = ifNear l (draw [soft (Foreground Blue)] 2 (column l) rpl) d 
                      $ drawSpan s e d a
  where l = argmin bytes s e

addFixit :: Delta -> Delta -> String -> Rendering -> Rendering
addFixit s e rpl r = drawFixit s e rpl .# r

data Fixit = Fixit 
  { fixitSpan        :: {-# UNPACK #-} !Span
  , fixitReplacement  :: {-# UNPACK #-} !ByteString 
  } deriving (Eq,Ord,Show)

instance Hashable Fixit where
  hash (Fixit s b) = hash s `hashWithSalt` b

instance Reducer Fixit Rendering where
  unit = render

instance Renderable Fixit where
  render (Fixit (Span s e bs) r) = addFixit s e (UTF8.toString r) $ rendering s bs

fixit :: MonadParser m => m Strict.ByteString -> m Fixit
fixit p = (\(r :~ s) -> Fixit s r) <$> spanned p

{-# LANGUAGE MultiParamTypeClasses #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Trifecta.Diagnostic.Rendering.Caret
-- Copyright   :  (C) 2011 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
----------------------------------------------------------------------------
module Text.Trifecta.Diagnostic.Rendering.Caret
  ( Caret(..)
  , caret
  , Careted(..)
  , careted
  -- * Internals
  , drawCaret
  , addCaret
  , caretEffects
  , renderingCaret
  ) where

import Control.Applicative
import Control.Comonad
import Data.ByteString (ByteString)
import Data.Foldable
import Data.Hashable
import Data.Semigroup
import Data.Semigroup.Reducer
import Data.Traversable
import Prelude hiding (span)
import System.Console.Terminfo.Color
import System.Console.Terminfo.PrettyPrint
import Text.Trifecta.Rope.Bytes
import Text.Trifecta.Rope.Delta
import Text.Trifecta.Parser.Class
import Text.Trifecta.Diagnostic.Rendering.Prim

-- |
-- > In file included from baz.c:9
-- > In file included from bar.c:4
-- > foo.c:8:36: note
-- > int main(int argc, char ** argv) { int; }
-- >                                    ^
data Caret = Caret !Delta {-# UNPACK #-} !ByteString deriving (Eq,Ord,Show)

instance Hashable Caret where
  hash (Caret d bs) = hash d `hashWithSalt` bs

caretEffects :: [ScopedEffect]
caretEffects = [soft (Foreground Green), soft Bold]

drawCaret :: Delta -> Delta -> Lines -> Lines
drawCaret p = ifNear p $ draw caretEffects 1 (fromIntegral (column p)) "^"

addCaret :: Delta -> Rendering -> Rendering
addCaret p r = drawCaret p .# r

caret :: MonadParser m => m Caret
caret = Caret <$> position <*> line

careted :: MonadParser m => m a -> m (Careted a)
careted p = do
  m <- position
  l <- line
  a <- p
  return $ a :^ Caret m l

instance HasBytes Caret where
  bytes = bytes . delta

instance HasDelta Caret where
  delta (Caret d _) = d

instance Renderable Caret where
  render (Caret d bs) = addCaret d $ rendering d bs

instance Reducer Caret Rendering where
  unit = render

instance Semigroup Caret where
  a <> _ = a

renderingCaret :: Delta -> ByteString -> Rendering
renderingCaret d bs = addCaret d $ rendering d bs

data Careted a = a :^ Caret deriving (Eq,Ord,Show)

instance Functor Careted where
  fmap f (a :^ s) = f a :^ s

instance HasDelta (Careted a) where
  delta (_ :^ c) = delta c

instance HasBytes (Careted a) where
  bytes (_ :^ c) = bytes c

instance Comonad Careted where
  extend f as@(_ :^ s) = f as :^ s
  extract (a :^ _) = a

instance Foldable Careted where
  foldMap f (a :^ _) = f a

instance Traversable Careted where
  traverse f (a :^ s) = (:^ s) <$> f a

instance Renderable (Careted a) where
  render (_ :^ a) = render a

instance Reducer (Careted a) Rendering where
  unit = render

instance Hashable a => Hashable (Careted a) where


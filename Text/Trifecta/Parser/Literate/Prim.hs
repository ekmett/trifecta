module Text.Trifecta.Parser.Literate.Prim
  ( LiterateState(..)
  , defaultLiterateState
  , LiterateMark
  ) where

import Data.Foldable
import Data.Traversable
import Data.Semigroup
import Data.Semigroup.Foldable
import Data.Semigroup.Traversable
import Text.Trifecta.Rope.Bytes
import Text.Trifecta.Rope.Delta

data LiterateState
  = Literate      -- ^ parsing a literate comment from a given position
  | LiterateCode  -- ^ In the midst of a @\begin{code} ... \end{code}@ block
  | LiterateTrack -- ^ In the midst of a @> ...@ block
  | Illiterate    -- ^ Don't use literate syntax

defaultLiterateState :: LiterateState
defaultLiterateState = literateStart

data LiterateMark = LiterateMark LiterateState d

instance Functor LiterateMark where
  fmap f (LiterateMark s a) = LiterateMark s (f a)

instance Foldable LiterateMark where
  foldMap f (LiterateMark _ a) = f a

instance Traversable LiterateMark where
  traverse f (LiterateMark s a) = LiterateMark s <$> f a

instance Foldable1 LiterateMark where
  foldMap1 f (LiterateMark _ a) = f a

instance Traversable1 LiterateMark where
  traverse1 f (LiterateMark s a) = LiterateMark s <$> f a

instance HasDelta d => (LiterateMark d) where
  delta (LiterateMark _ d) = delta d

instance HasBytes d => (LiterateMark d) where
  bytes (LiterateMark _ d) = bytes d

instance Semigroup d => Semigroup (LiterateMark d) where
  LiterateMark _ d <> LiterateMark s e = LiterateMark s (d <> e)

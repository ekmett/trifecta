module Text.Trifecta.Util.MaybePair
  ( MaybePair(..)
  ) where

import Control.Applicative
import Data.Bifunctor
import Data.Bifoldable
import Data.Bitraversable
import Data.Foldable
import Data.Traversable

data MaybePair a b = JustPair a b | NothingPair

instance Bifunctor MaybePair where
  bimap f g (JustPair a b) = JustPair (f a) (g b)
  bimap _ _ NothingPair = NothingPair
 
instance Functor (MaybePair a) where
  fmap f (JustPair a b) = JustPair a (f b)
  fmap _ NothingPair = NothingPair

instance Foldable (MaybePair a) where
  foldMap f (JustPair _ b) = f b
  foldMap _ NothingPair = mempty

instance Traversable (MaybePair a) where
  traverse f (JustPair a b) = JustPair a <$> f b
  traverse _ NothingPair = pure NothingPair

instance Bifoldable MaybePair where
  bifoldMap f g (JustPair a b) = f a `mappend` g b
  bifoldMap _ _ NothingPair = mempty

instance Bitraversable MaybePair where
  bitraverse f g (JustPair a b) = JustPair <$> f a <*> g b
  bitraverse _ _ NothingPair = pure NothingPair

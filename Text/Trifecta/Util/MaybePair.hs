module Text.Trifecta.Util.MaybePair
  ( MaybePair(..)
  ) where

import Control.Applicative
import Data.Semigroup
import Data.Monoid
import Data.Functor.Apply
import Data.Functor.Plus
import Data.Bifunctor
import Data.Bifoldable
import Data.Bitraversable
import Data.Foldable
import Data.Traversable

data MaybePair a b = JustPair a b | NothingPair
  deriving (Eq,Ord,Show,Read)

instance (Semigroup a, Semigroup b) => Semigroup (MaybePair a b) where
  a <> NothingPair = a
  NothingPair <> b = b
  JustPair a b <> JustPair c d = JustPair (a <> c) (b <> d)

instance (Semigroup a, Semigroup b) => Monoid (MaybePair a b) where
  mappend = (<>) 
  mempty = NothingPair

instance Bifunctor MaybePair where
  bimap f g (JustPair a b) = JustPair (f a) (g b)
  bimap _ _ NothingPair = NothingPair
 
instance Functor (MaybePair a) where
  fmap f (JustPair a b) = JustPair a (f b)
  fmap _ NothingPair = NothingPair

instance Semigroup a => Apply (MaybePair a) where
  JustPair a b <.> JustPair c d = JustPair (a <> c) (b d)
  _ <.> _ = NothingPair

instance Semigroup a => Alt (MaybePair a) where
  a <!> NothingPair = a
  NothingPair <!> b = b
  JustPair a b <!> JustPair c _ = JustPair (a <> c) b

instance Semigroup a => Plus (MaybePair a) where
  zero = NothingPair

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

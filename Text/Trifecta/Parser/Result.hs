module Text.Trifecta.Parser.Result 
  ( Result(..)
  ) where

import Control.Applicative
import Data.Monoid
import Data.Semigroup
import Data.Foldable
import Data.Functor.Apply
import Data.Functor.Plus
import Data.Traversable
import Data.Bifunctor
import Data.Sequence
import Text.Trifecta.Diagnostic

data Result e a
  = Success !(Seq (Diagnostic e)) a
  | Failure !(Seq (Diagnostic e)) !(Diagnostic e)

instance Functor (Result e) where
  fmap f (Success xs a) = Success xs (f a)
  fmap _ (Failure xs e) = Failure xs e

instance Bifunctor Result where
  bimap f g (Success xs a) = Success (fmap (fmap f) xs) (g a)
  bimap f g (Failure xs e) = Failure (fmap (fmap f) xs) (fmap f e)

instance Foldable (Result e) where
  foldMap f (Success _ a) = f a
  foldMap _ (Failure _ _) = mempty

instance Traversable (Result e) where
  traverse f (Success xs a) = Success xs <$> f a
  traverse _ (Failure xs e) = pure $ Failure xs e

instance Applicative (Result e) where
  pure = Success mempty
  Success xs f <*> Success ys a  = Success (xs <> ys) (f a)
  Success xs _ <*> Failure ys e  = Failure (xs <> ys) e
  Failure xs e <*> Success ys _  = Failure (xs <> ys) e
  Failure xs e <*> Failure ys e' = Failure (xs <> ys |> e) e'

instance Apply (Result e) where
  (<.>) = (<*>)

{-
instance Alt (Result e) where
  Failure xs e <!> Failure ys e' = Failure (xs <> ys |> e) e'
  Success xs a <!> Success ys _  = Success (xs <> ys) a
  Success xs a <!> Failure ys e  = Success (xs <> ys) a
  Failure xs _ <!> Success ys a  = Success (xs <> ys |> e) a

instance Plus (Result e) where
  zero = Failure mempty zero

instance Alternative (Result e) where 
  (<|>) = (<!>)
  empty = zero
-}


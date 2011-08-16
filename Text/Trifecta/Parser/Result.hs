module Text.Trifecta.Parser.Result 
  ( Result(..)
  ) where

import Control.Applicative
import Data.Semigroup
import Data.Foldable
import Data.Functor.Apply
import Data.Functor.Plus
import Data.Traversable
import Data.Bifunctor
import Data.Sequence as Seq
import Text.Trifecta.Diagnostic.Prim
import Text.PrettyPrint.Free
import System.Console.Terminfo.PrettyPrint

data Result e a
  = Success !(Seq (Diagnostic e)) a
  | Failure !(Seq (Diagnostic e))
  deriving Show

instance (Pretty e, Show a) => Pretty (Result e a) where
  pretty (Success xs a) 
    | Seq.null xs = string (show a)
    | otherwise   = prettyList (toList xs) `above` string (show a)
  pretty (Failure xs) = prettyList $ toList xs

instance (PrettyTerm e, Show a) => PrettyTerm (Result e a) where
  prettyTerm (Success xs a)
    | Seq.null xs = string (show a)
    | otherwise   = prettyTermList (toList xs) `above` string (show a)
  prettyTerm (Failure xs) = prettyTermList $ toList xs

instance Functor (Result e) where
  fmap f (Success xs a) = Success xs (f a)
  fmap _ (Failure xs) = Failure xs

instance Bifunctor Result where
  bimap f g (Success xs a) = Success (fmap (fmap f) xs) (g a)
  bimap f _ (Failure xs) = Failure (fmap (fmap f) xs)

instance Foldable (Result e) where
  foldMap f (Success _ a) = f a
  foldMap _ (Failure _) = mempty

instance Traversable (Result e) where
  traverse f (Success xs a) = Success xs <$> f a
  traverse _ (Failure xs) = pure $ Failure xs

instance Applicative (Result e) where
  pure = Success mempty
  Success xs f <*> Success ys a = Success (xs <> ys) (f a)
  Success xs _ <*> Failure ys   = Failure (xs <> ys)
  Failure xs   <*> Success ys _ = Failure (xs <> ys)
  Failure xs   <*> Failure ys   = Failure (xs <> ys)

instance Apply (Result e) where
  (<.>) = (<*>)

instance Alt (Result e) where
  Failure xs   <!> Failure ys    = Failure (xs <> ys)
  Success xs a <!> Success ys _  = Success (xs <> ys) a
  Success xs a <!> Failure ys    = Success (xs <> ys) a
  Failure xs   <!> Success ys a  = Success (xs <> ys) a

instance Plus (Result e) where
  zero = Failure mempty

instance Alternative (Result e) where 
  (<|>) = (<!>)
  empty = zero

-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Trifecta.Diagnostic.Err
-- Copyright   :  (C) 2011 Edward Kmett,
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- The unlocated error type used internally within the parser.
----------------------------------------------------------------------------
module Text.Trifecta.Diagnostic.Err
  ( Err(..)
  , knownErr
  , fatalErr
  ) where

import Control.Applicative
import Data.Foldable
import Data.Traversable
import Data.Semigroup
import Data.Functor.Plus
import Text.Trifecta.Diagnostic.Prim
import Text.Trifecta.Diagnostic.Level
import Text.Trifecta.Diagnostic.Rendering.Prim
import Text.PrettyPrint.Free

data Err e
  = EmptyErr                  -- no error specified, unlocated
  | FailErr  Rendering String -- a recoverable error caused by fail from a known location
  | PanicErr Rendering String -- something is bad with the grammar, fail fast
  | Err     !(Diagnostic e)   -- a user defined error message
  deriving Show

knownErr :: Err e -> Bool
knownErr EmptyErr = False
knownErr _ = True

fatalErr :: Err e -> Bool
fatalErr (Err (Diagnostic _ Panic _ _)) = True
fatalErr (Err (Diagnostic _ Fatal _ _)) = True
fatalErr (PanicErr _ _) = True
fatalErr _ = False

instance Functor Err where
  fmap _ EmptyErr = EmptyErr
  fmap _ (FailErr r s) = FailErr r s
  fmap _ (PanicErr r s) = PanicErr r s
  fmap f (Err e) = Err (fmap f e)

instance Foldable Err where
  foldMap _ EmptyErr   = mempty
  foldMap _ FailErr{}  = mempty
  foldMap _ PanicErr{} = mempty
  foldMap f (Err e) = foldMap f e

instance Traversable Err where
  traverse _ EmptyErr = pure EmptyErr
  traverse _ (FailErr r s) = pure $ FailErr r s
  traverse _ (PanicErr r s) = pure $ PanicErr r s
  traverse f (Err e) = Err <$> traverse f e

-- | Merge two errors, selecting the most severe.
instance Alt Err where
  a <!> EmptyErr            = a
  _ <!> a@(Err (Diagnostic _ Panic _ _)) = a
  a@(Err (Diagnostic _ Panic _ _)) <!> _ = a
  _ <!> a@PanicErr{} = a
  a@PanicErr{} <!> _ = a
  _ <!> a@(Err (Diagnostic _ Fatal _ _)) = a
  a@(Err (Diagnostic _ Fatal _ _)) <!> _ = a
  _ <!> b = b
  {-# INLINE (<!>) #-}

-- | Merge two errors, selecting the most severe.
instance Plus Err where
  zero = EmptyErr

-- | Merge two errors, selecting the most severe.
instance Semigroup (Err t) where
  (<>) = (<!>)
  times1p _ = id

-- | Merge two errors, selecting the most severe.
instance Monoid (Err t) where
  mempty = EmptyErr
  mappend = (<!>)

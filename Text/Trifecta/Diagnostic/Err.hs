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

import Data.Semigroup
import Data.Functor.Plus
import Text.Trifecta.Diagnostic.Prim
import Text.Trifecta.Diagnostic.Level
import Text.Trifecta.Diagnostic.Rendering.Prim
import Text.PrettyPrint.Free

-- | unlocated error
data Err e
  = EmptyErr         -- empty, no error specified
  | FailErr String   -- a recoverable error caused by fail
  | PanicErr String  -- something is bad with the grammar, fail fast
  | Err     [Rendering] !DiagnosticLevel e [Diagnostic e]
  deriving Show

knownErr :: Err e -> Bool
knownErr EmptyErr = False
knownErr _ = True

fatalErr :: Err e -> Bool
fatalErr (Err _ Fatal _ _) = True
fatalErr (PanicErr _) = True
fatalErr _ = False

instance Functor Err where
  fmap _ EmptyErr = EmptyErr
  fmap _ (FailErr s) = FailErr s
  fmap _ (PanicErr s) = PanicErr s
  fmap f (Err rs l e es) = Err rs l (f e) (fmap (fmap f) es)

instance Alt Err where
  a                   <!> EmptyErr            = a
  _                   <!> a@(Err _ Fatal _ _) = a
  a@(Err _ Fatal _ _) <!> _                   = a
  _                   <!> a@PanicErr{}        = a
  a@PanicErr{}        <!> _                   = a
  _                   <!> b                   = b
  {-# INLINE (<!>) #-}

instance Plus Err where
  zero = EmptyErr

instance Semigroup (Err t) where
  (<>) = (<!>)
  times1p _ = id

instance Monoid (Err t) where
  mempty = EmptyErr
  mappend = (<!>) 

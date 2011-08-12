module Text.Trifecta.Diagnostic.Err
  ( Err(..)
  , knownErr
  , fatalErr
  ) where

import Data.Semigroup
import Data.Monoid
import Data.Functor.Plus
import Text.Trifecta.Diagnostic.Prim
import Text.Trifecta.Diagnostic.Level
import Text.Trifecta.Diagnostic.Rendering.Prim
import Text.PrettyPrint.Free

-- | unlocated error
data Err e
  = EmptyErr
  | FailErr String
  | Err     [Rendering] !DiagnosticLevel e [Diagnostic e]
  deriving Show

knownErr :: Err e -> Bool
knownErr EmptyErr = False
knownErr _ = True

fatalErr :: Err e -> Bool
fatalErr (Err _ Fatal _ _) = True
fatalErr _ = False

instance Functor Err where
  fmap _ EmptyErr = EmptyErr
  fmap _ (FailErr s) = FailErr s
  fmap f (Err rs l e es) = Err rs l (f e) (fmap (fmap f) es)

instance Alt Err where
  a                   <!> EmptyErr            = a
  _                   <!> a@(Err _ Fatal _ _) = a
  a@(Err _ Fatal _ _) <!> _                   = a
  _                   <!> b                   = b
  {-# INLINE (<!>) #-}

instance Plus Err where
  zero = EmptyErr

instance Semigroup (Err t) where
  (<>) = (<!>)
  replicate1p _ = id

instance Monoid (Err t) where
  mempty = EmptyErr
  mappend = (<!>) 

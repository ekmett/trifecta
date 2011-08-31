-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Trifecta.Diagnostic.Err.State
-- Copyright   :  (C) 2011 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
----------------------------------------------------------------------------
module Text.Trifecta.Diagnostic.Err.State 
  ( ErrState(..)
  ) where

import Data.Functor.Plus
import Data.Set as Set
import Data.Semigroup
import Text.PrettyPrint.Free
import Text.Trifecta.Diagnostic.Err
import Text.Trifecta.Diagnostic.Rendering.Caret

data ErrState e = ErrState
 { errExpected  :: !(Set (Careted String))
 , errMessage   :: !(Err e)
 }

instance Functor ErrState where
  fmap f (ErrState a b) = ErrState a (fmap f b)

instance Alt ErrState where
  ErrState a b <!> ErrState a' b' = ErrState (a <> a') (b <> b')
  {-# INLINE (<!>) #-}

instance Plus ErrState where
  zero = ErrState mempty mempty
 
instance Semigroup (ErrState e) where
  (<>) = (<!>) 

instance Monoid (ErrState e) where
  mempty = zero
  mappend = (<!>)

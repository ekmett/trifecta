-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Trifecta.Diagnostic.Err.Log
-- Copyright   :  (C) 2011 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
----------------------------------------------------------------------------
module Text.Trifecta.Diagnostic.Err.Log
  ( ErrLog(..)
  ) where

import Data.Functor.Plus
import Data.Semigroup
import Text.Trifecta.Diagnostic.Prim
import Text.Trifecta.Highlight.Prim
import Data.Semigroup.Union (union, empty)
import Data.Sequence (Seq)

data ErrLog e = ErrLog
  { errLog        :: !(Seq (Diagnostic e))
  , errHighlights :: !Highlights
  }

instance Functor ErrLog where
  fmap f (ErrLog a b) = ErrLog (fmap (fmap f) a) b

instance Alt ErrLog where
  ErrLog a b <!> ErrLog a' b' = ErrLog (a <> a') (union b b')
  {-# INLINE (<!>) #-}

instance Plus ErrLog where
  zero = ErrLog mempty empty
 
instance Semigroup (ErrLog e) where
  (<>) = (<!>) 

instance Monoid (ErrLog e) where
  mempty = zero
  mappend = (<!>)

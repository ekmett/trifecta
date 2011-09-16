{-# LANGUAGE MultiParamTypeClasses #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Trifecta.Layout.Prim
-- Copyright   :  (C) 2011 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
----------------------------------------------------------------------------
module Text.Trifecta.Layout.Prim
  ( LayoutToken(..)
  , LayoutState(..)
  , LayoutContext(..)
  , LayoutMark(..)
  , defaultLayoutState
  , layoutBol
  , layoutStack
  ) where

import Data.Functor
import Data.Lens.Common
import Data.Foldable
import Data.Semigroup.Foldable
import Data.Semigroup.Traversable
import Data.Traversable
import Text.Trifecta.Rope.Delta
import Text.Trifecta.Rope.Bytes
import Text.Trifecta.Diagnostic.Rendering.Prim

data LayoutToken
  = VirtualSemi
  | VirtualRightBrace
  | WhiteSpace
  | Other
  deriving (Eq,Ord,Show,Read)

data LayoutContext
  = IndentedLayout Rendering
  | DisabledLayout Rendering

instance HasDelta LayoutContext where
  delta (IndentedLayout r) = delta r
  delta (DisabledLayout r) = delta r

instance HasBytes LayoutContext where
  bytes = bytes . delta

data LayoutState = LayoutState
  { _layoutBol      :: Bool
  , _layoutStack    :: [LayoutContext]
  }

defaultLayoutState :: LayoutState
defaultLayoutState = LayoutState False []

layoutBol :: Lens LayoutState Bool
layoutBol = lens _layoutBol (\s l -> l { _layoutBol = s})

layoutStack :: Lens LayoutState [LayoutContext]
layoutStack = lens _layoutStack (\s l -> l { _layoutStack = s})

data LayoutMark d = LayoutMark LayoutState d

instance Functor LayoutMark where
  fmap f (LayoutMark s a) = LayoutMark s (f a)

instance Foldable LayoutMark where
  foldMap f (LayoutMark _ a) = f a

instance Traversable LayoutMark where
  traverse f (LayoutMark s a) = LayoutMark s <$> f a

instance Foldable1 LayoutMark where
  foldMap1 f (LayoutMark _ a) = f a

instance Traversable1 LayoutMark where
  traverse1 f (LayoutMark s a) = LayoutMark s <$> f a

instance HasDelta d => HasDelta (LayoutMark d) where
  delta (LayoutMark _ d) = delta d

instance HasBytes d => HasBytes (LayoutMark d) where
  bytes (LayoutMark _ d) = bytes d

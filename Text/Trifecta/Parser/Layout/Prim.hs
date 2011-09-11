{-# LANGUAGE MultiParamTypeClasses #-}
module Text.Trifecta.Parser.Layout.Prim
  ( LayoutToken(..)
  , LayoutState(..)
  , LayoutContext(..)
  , defaultLayoutState
  , layoutBol
  , layoutStack
  ) where

import Data.Lens.Common
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

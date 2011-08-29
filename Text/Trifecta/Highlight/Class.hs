module Text.Trifecta.Highlight.Class 
  ( Highlightable(..)
  ) where

import Text.Trifecta.Highlight.Prim

class Highlightable a where
  addHighlights :: Highlights -> a -> a

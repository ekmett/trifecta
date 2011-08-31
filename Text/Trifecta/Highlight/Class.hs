-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Trifecta.Highlight.Class
-- Copyright   :  (C) 2011 Edward Kmett,
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
----------------------------------------------------------------------------
module Text.Trifecta.Highlight.Class 
  ( Highlightable(..)
  ) where

import Text.Trifecta.Highlight.Prim

class Highlightable a where
  addHighlights :: Highlights -> a -> a

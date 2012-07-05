-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Trifecta.Highlight
-- Copyright   :  (C) 2011 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
----------------------------------------------------------------------------
module Text.Trifecta.Highlight 
  ( 
  -- * Text.Trifecta.Highlight.Class
    Highlightable(..)
  -- * Text.Trifecta.Highlight.Prim
  , Highlight
  , Highlights
  ) where

import Text.Trifecta.Highlight.Class
import Text.Trifecta.Highlight.Prim

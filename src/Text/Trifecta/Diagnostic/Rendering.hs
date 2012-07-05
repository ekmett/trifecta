-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Trifecta.Diagnostic.Rendering
-- Copyright   :  (C) 2011 Edward Kmett,
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
----------------------------------------------------------------------------
module Text.Trifecta.Diagnostic.Rendering
  ( Renderable(..)
  , Source, rendering, renderingCaret
  , Caret(..), Careted(..)
  , Span(..), Spanned(..)
  , Fixit(..), Rendered(..)
  ) where

import Text.Trifecta.Diagnostic.Rendering.Prim
import Text.Trifecta.Diagnostic.Rendering.Caret
import Text.Trifecta.Diagnostic.Rendering.Span
import Text.Trifecta.Diagnostic.Rendering.Fixit

-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Trifecta.Diagnostic
-- Copyright   :  (C) 2011 Edward Kmett,
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
----------------------------------------------------------------------------
module Text.Trifecta.Diagnostic 
  ( 
  -- * Diagnostics
    Diagnostic(..)
  -- * Rendering
  , Renderable(..)
  , Source
  , rendering
  , renderingCaret
  , Caret(..), Careted(..)
  , Span(..), Spanned(..)
  , Fixit(..), Rendered(..)
  -- * Emitting diagnostics
  , MonadDiagnostic(..)
  , panic, panicAt
  , fatal, fatalAt
  , err, errAt
  , warn, warnAt
  , note, noteAt
  , verbose, verboseAt
  -- * Diagnostic Levels
  , DiagnosticLevel(..)
  ) where

import Text.Trifecta.Diagnostic.Prim
import Text.Trifecta.Diagnostic.Class
import Text.Trifecta.Diagnostic.Combinators
import Text.Trifecta.Diagnostic.Level
import Text.Trifecta.Diagnostic.Rendering

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
  , tellDiagnostic
  -- * Rendering
  , Renderable(..)
  , Source
  , rendering
  , Caret(..), Careted(..)
  , Span(..), Spanned(..)
  , Fixit(..), Rendered(..)
  -- * Emitting diagnostics
  , MonadDiagnostic(..)
  , fatal
  , err
  , warn
  , note
  , verbose
  , warnWith
  , noteWith
  , verboseWith
  -- * Diagnostic Levels
  , DiagnosticLevel(..)
  ) where

import Text.Trifecta.Diagnostic.Prim
import Text.Trifecta.Diagnostic.Class
import Text.Trifecta.Diagnostic.Combinators
import Text.Trifecta.Diagnostic.Level
import Text.Trifecta.Diagnostic.Rendering

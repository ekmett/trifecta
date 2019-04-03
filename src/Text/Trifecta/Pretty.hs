-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2011-2015 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- An interface between the prettyprinter and the ANSI SGR primitives
-- provided by the ansi-terminal package.
----------------------------------------------------------------------------

module Text.Trifecta.Pretty
  ( ANSIPretty (..)
  , AnsiStyle
  , renderIO
  -- * Rendering
  , char
  -- * Styles
  , bold
  , debold
  , underlined
  , deunderline
  -- * Compatibility shims
  , renderPretty
  , columns
  ) where

import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Terminal
import Data.Text.Prettyprint.Doc.Render.Terminal.Internal (ansiBold, ansiUnderlining)

class ANSIPretty a where
  apretty :: a -> Doc AnsiStyle

char :: Char -> Doc a
char = pretty

renderPretty :: Double -> Int -> Doc AnsiStyle -> SimpleDocStream AnsiStyle
renderPretty ribbonFraction page
  = layoutSmart LayoutOptions { layoutPageWidth = AvailablePerLine page ribbonFraction }

debold, deunderline :: AnsiStyle
debold = mempty { ansiBold = Nothing }
deunderline = mempty { ansiUnderlining = Nothing}

columns :: (Maybe Int -> Doc AnsiStyle) -> Doc AnsiStyle
columns f = pageWidth (f . toMaybeInt) where
  toMaybeInt (AvailablePerLine cpl _) = Just cpl
  toMaybeInt Unbounded = Nothing

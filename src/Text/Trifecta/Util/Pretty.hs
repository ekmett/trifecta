-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2011-2019 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Utility functions to augment the prettyprinter library's interface.
----------------------------------------------------------------------------

module Text.Trifecta.Util.Pretty
  ( AnsiStyle
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


import Prettyprinter
import Prettyprinter.Render.Terminal
import Prettyprinter.Render.Terminal.Internal (ansiBold, ansiUnderlining)

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

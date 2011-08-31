-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Trifecta.Highlight.Effects
-- Copyright   :  (C) 2011 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
----------------------------------------------------------------------------
module Text.Trifecta.Highlight.Effects 
  ( highlightEffects
  , pushToken
  , popToken
  , withHighlight
  ) where

import Control.Applicative
import System.Console.Terminfo.PrettyPrint
import System.Console.Terminfo.Color
import Data.Semigroup
import Text.Trifecta.Highlight.Prim

highlightEffects :: Highlight -> [ScopedEffect]
highlightEffects Comment            = [soft $ Foreground Green]
highlightEffects ReservedIdentifier = [soft $ Foreground Blue]
highlightEffects Operator           = [soft $ Foreground Yellow]
highlightEffects ReservedOperator   = [soft $ Foreground Yellow]
highlightEffects EscapeCode         = [soft $ Foreground Magenta, soft Bold]
highlightEffects CharLiteral        = [soft $ Foreground Cyan]
highlightEffects StringLiteral      = [soft $ Foreground Cyan]
-- highlightEffects Identifier      = []
highlightEffects _             = []

pushToken, popToken :: Highlight -> TermDoc
pushToken h = foldr (\a b -> pure (Push a) <> b) mempty (highlightEffects h)
popToken h  = foldr (\_ b -> pure Pop      <> b) mempty (highlightEffects h)

withHighlight :: Highlight -> TermDoc -> TermDoc
withHighlight h d = pushToken h <> d <> popToken h

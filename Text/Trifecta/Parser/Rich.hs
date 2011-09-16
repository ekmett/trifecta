-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Trifecta.Parser.Rich
-- Copyright   :  (C) 2011 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
----------------------------------------------------------------------------
module Text.Trifecta.Parser.Rich
  ( Rich
  , rich
  ) where

import Control.Monad (liftM)
import Text.Trifecta.Parser.Layout
import Text.Trifecta.Parser.Language
import Text.Trifecta.Parser.Language.Prim
import Text.Trifecta.Parser.Literate

type Rich m = Layout (Language (Literate m))

rich :: Monad m => LiterateState -> LanguageDef m -> Rich m a -> m (a, LiterateState)
rich lit def p = runLiterate (runLanguage (fst `liftM` runLayout p defaultLayoutState) (liftLanguageDef (liftLanguageDef def))) lit

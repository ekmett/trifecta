-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Trifecta.Parser.Language
-- Copyright   :  (C) 2011 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
----------------------------------------------------------------------------
module Text.Trifecta.Parser.Language
  ( Language(..)
  , runLanguage
  , LanguageDef(..)
  , MonadLanguage(..)
  , asksLanguage
  , identifier
  , reserved
  , reservedByteString
  , op
  , reservedOp
  , reservedOpByteString
  , emptyLanguageDef
  , haskellLanguageDef
  , haskell98LanguageDef
  ) where

import Text.Trifecta.Parser.Language.Class
import Text.Trifecta.Parser.Language.Combinators
import Text.Trifecta.Parser.Language.Def
import Text.Trifecta.Parser.Language.Monad
import Text.Trifecta.Parser.Language.Style


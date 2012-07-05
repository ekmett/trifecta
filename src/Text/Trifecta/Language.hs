-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Trifecta.Language
-- Copyright   :  (C) 2011 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
----------------------------------------------------------------------------
module Text.Trifecta.Language
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

import Text.Trifecta.Language.Class
import Text.Trifecta.Language.Combinators
import Text.Trifecta.Language.Prim
import Text.Trifecta.Language.Monad
import Text.Trifecta.Language.Style


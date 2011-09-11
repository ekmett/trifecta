-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Trifecta.Parser.Language.Combinators
-- Copyright   :  (C) 2011 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
----------------------------------------------------------------------------
module Text.Trifecta.Parser.Language.Combinators
  ( identifier
  , reserved
  , reservedByteString
  , op
  , reservedOp
  , reservedOpByteString
  ) where

import Data.ByteString
import Text.Trifecta.Parser.Language.Class
import Text.Trifecta.Parser.Language.Def
import Text.Trifecta.Parser.Token.Identifier

identifier :: MonadLanguage m => m ByteString
identifier = asksLanguage languageIdentifiers >>= ident

reserved :: MonadLanguage m => String -> m ()
reserved i = asksLanguage languageIdentifiers >>= \style -> reserve style i

reservedByteString :: MonadLanguage m => ByteString -> m ()
reservedByteString i = asksLanguage languageIdentifiers >>= \style -> reserveByteString style i

op :: MonadLanguage m => m ByteString
op = asksLanguage languageOperators >>= ident

reservedOp :: MonadLanguage m => String -> m ()
reservedOp i = asksLanguage languageOperators >>= \style -> reserve style i

reservedOpByteString :: MonadLanguage m => ByteString -> m ()
reservedOpByteString i = asksLanguage languageOperators >>= \style -> reserveByteString style i

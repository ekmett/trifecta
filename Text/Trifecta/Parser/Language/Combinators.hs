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
import Text.Trifecta.Parser.Language.Prim
import Text.Trifecta.Parser.Identifier

identifier :: MonadLanguage m => m ByteString
identifier = asksLanguage languageIdentifierStyle >>= ident

reserved :: MonadLanguage m => String -> m ()
reserved i = asksLanguage languageIdentifierStyle >>= \style -> reserve style i

reservedByteString :: MonadLanguage m => ByteString -> m ()
reservedByteString i = asksLanguage languageIdentifierStyle >>= \style -> reserveByteString style i

op :: MonadLanguage m => m ByteString
op = asksLanguage languageOperatorStyle >>= ident

reservedOp :: MonadLanguage m => String -> m ()
reservedOp i = asksLanguage languageOperatorStyle >>= \style -> reserve style i

reservedOpByteString :: MonadLanguage m => ByteString -> m ()
reservedOpByteString i = asksLanguage languageOperatorStyle >>= \style -> reserveByteString style i

-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Trifecta.Language.Prim
-- Copyright   :  (C) 2011 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
----------------------------------------------------------------------------
module Text.Trifecta.Language.Prim
  ( LanguageDef(..)
  , liftLanguageDef
  ) where

import Control.Monad.Trans.Class
import Text.Trifecta.Parser.Token.Style
import Text.Trifecta.Parser.Identifier

data LanguageDef m = LanguageDef
  { languageCommentStyle     :: CommentStyle
  , languageIdentifierStyle  :: IdentifierStyle m
  , languageOperatorStyle    :: IdentifierStyle m
  }

liftLanguageDef :: (MonadTrans t, Monad m) => LanguageDef m -> LanguageDef (t m)
liftLanguageDef (LanguageDef c i o) = LanguageDef c (liftIdentifierStyle i) (liftIdentifierStyle o)

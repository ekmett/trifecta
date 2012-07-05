-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Trifecta.Language.Style
-- Copyright   :  (c) Edward Kmett 2011
-- License     :  BSD3
-- 
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  provisional
-- Portability :  non-portable
-- 
-----------------------------------------------------------------------------
module Text.Trifecta.Language.Style
  ( emptyLanguageDef
  , haskellLanguageDef
  , haskell98LanguageDef
  ) where

import Text.Trifecta.Parser.Class
import Text.Trifecta.Parser.Token.Style
import Text.Trifecta.Parser.Identifier.Style
import Text.Trifecta.Language.Prim

emptyLanguageDef, haskellLanguageDef, haskell98LanguageDef :: MonadParser m => LanguageDef m
emptyLanguageDef     = LanguageDef emptyCommentStyle   emptyIdents     emptyOps
haskellLanguageDef   = LanguageDef haskellCommentStyle haskellIdents   haskellOps
haskell98LanguageDef = LanguageDef haskellCommentStyle haskell98Idents haskell98Ops

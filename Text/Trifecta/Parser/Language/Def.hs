module Text.Trifecta.Parser.Language.Def
  ( LanguageDef(..)
  , liftLanguageDef
  ) where

import Control.Monad.Trans.Class
import Text.Trifecta.Parser.Token.Style
import Text.Trifecta.Parser.Identifier

data LanguageDef m = LanguageDef
  { languageCommentStyle :: CommentStyle
  , languageIdentifiers  :: IdentifierStyle m
  , languageOperators    :: IdentifierStyle m
  }

liftLanguageDef :: (MonadTrans t, Monad m) => LanguageDef m -> LanguageDef (t m)
liftLanguageDef (LanguageDef c i o) = LanguageDef c (liftIdentifierStyle i) (liftIdentifierStyle o)

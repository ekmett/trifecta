module Text.Trifecta.Parser.Language.Prim
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

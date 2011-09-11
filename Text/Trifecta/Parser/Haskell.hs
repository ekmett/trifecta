module Text.Trifecta.Parser.Haskell
  ( Haskell
  , haskell
  , haskell98
  ) where

import Data.Functor
import Text.Trifecta.Parser.Class
import Text.Trifecta.Parser.Layout
import Text.Trifecta.Parser.Language

type Haskell m = Layout (Language m)

haskell :: MonadParser m => Haskell m a -> m a
haskell p = runLanguage (fst <$> runLayout p defaultLayoutState) haskellLanguageDef

haskell98 :: MonadParser m => Haskell m a -> m a
haskell98 p = runLanguage (fst <$> runLayout p defaultLayoutState) haskell98LanguageDef

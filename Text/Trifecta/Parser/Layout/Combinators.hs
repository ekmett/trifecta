{-# LANGUAGE MultiParamTypeClasses #-}
module Text.Trifecta.Parser.Layout.Combinators
  ( layoutEq
  , disableLayout
  , enableLayout
  , laidout
  ) where

import Control.Applicative
import Control.Monad (guard)
import Text.Trifecta.Rope.Delta
import Text.Trifecta.Parser.Class
import Text.Trifecta.Parser.Token.Combinators
import qualified Text.Trifecta.Highlight.Prim as Highlight
import Text.Trifecta.Parser.Layout.Class
import Text.Trifecta.Parser.Layout.Prim

disableLayout :: MonadLayoutParser m => m a -> m a
disableLayout p = do
  r <- rend
  modLayout layoutStack (DisabledLayout r:)
  result <- p
  stk <- getLayout layoutStack
  case stk of
    DisabledLayout r':xs | delta r == delta r' -> result <$ setLayout layoutStack xs
    _ -> unexpected "layout"

enableLayout :: MonadLayoutParser m => m a -> m a
enableLayout p = do
  result <- highlight Highlight.Layout $ do
    r <- rend
    modLayout layoutStack (IndentedLayout r:)
    p
  result <$ layout <?> "virtual right brace"

laidout :: MonadLayoutParser m => m a -> m a
laidout p = braces p <|> enableLayout p

layoutEq :: MonadLayoutParser m => LayoutToken -> m LayoutToken
layoutEq s = try $ do
  r <- layout
  guard (s == r)
  return r


{-# LANGUAGE MultiParamTypeClasses #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Trifecta.Layout.Combinators
-- Copyright   :  (C) 2011 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
----------------------------------------------------------------------------
module Text.Trifecta.Layout.Combinators
  ( layoutEq
  , getLayout
  , setLayout
  , modLayout
  , disableLayout
  , enableLayout
  , laidout
  ) where

import Control.Applicative
import Control.Monad (guard)
import Data.Lens.Common
import Text.Trifecta.Rope.Delta
import Text.Trifecta.Parser.Class
import Text.Trifecta.Parser.Token.Combinators
import qualified Text.Trifecta.Highlight.Prim as Highlight
import Text.Trifecta.Layout.Class
import Text.Trifecta.Layout.Prim

getLayout :: MonadLayout m => Lens LayoutState t -> m t
getLayout l = layoutState $ \s -> (getL l s, s)

setLayout :: MonadLayout m => Lens LayoutState t -> t -> m ()
setLayout l t = layoutState $ \s -> ((), setL l t s)

modLayout :: MonadLayout m => Lens LayoutState t -> (t -> t) -> m ()
modLayout l f = layoutState $ \s -> ((), modL l f s)

disableLayout :: MonadLayout m => m a -> m a
disableLayout p = do
  r <- rend
  modLayout layoutStack (DisabledLayout r:)
  result <- p
  stk <- getLayout layoutStack
  case stk of
    DisabledLayout r':xs | delta r == delta r' -> result <$ setLayout layoutStack xs
    _ -> unexpected "layout"

enableLayout :: MonadLayout m => m a -> m a
enableLayout p = do
  result <- highlight Highlight.Layout $ do
    r <- rend
    modLayout layoutStack (IndentedLayout r:)
    p
  result <$ layout <?> "virtual right brace"

laidout :: MonadLayout m => m a -> m a
laidout p = braces p <|> enableLayout p

layoutEq :: MonadLayout m => LayoutToken -> m ()
layoutEq s = try $ do
  r <- layout
  guard (s == r)


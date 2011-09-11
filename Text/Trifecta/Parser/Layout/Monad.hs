{-# LANGUAGE MultiParamTypeClasses, GeneralizedNewtypeDeriving, FlexibleInstances, UndecidableInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Trifecta.Parser.Layout.Monad
-- Copyright   :  (C) 2011 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
----------------------------------------------------------------------------
module Text.Trifecta.Parser.Layout.Monad
  ( Layout(..)
  , runLayout
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.State.Class
import Control.Monad.Trans.State.Strict (StateT(..))
import Control.Monad.Writer.Class
import Control.Monad.Reader.Class
import Control.Monad.Cont.Class
import Control.Monad.Trans.Class
import Data.Lens
import Text.Trifecta.Diagnostic.Class
import Text.Trifecta.Parser.Class
import Text.Trifecta.Parser.Combinators
import Text.Trifecta.Parser.Token.Class
import Text.Trifecta.Parser.Layout.Prim
import Text.Trifecta.Parser.Layout.Class
import Text.Trifecta.Parser.Layout.Combinators
import Text.Trifecta.Rope.Delta

newtype Layout m a = Layout { unlayout :: StateT LayoutState m a }
  deriving (Functor, Applicative, Alternative, Monad, MonadPlus, MonadTrans, MonadCont)

runLayout :: Monad m => Layout m a -> LayoutState -> m (a, LayoutState)
runLayout = runStateT . unlayout

instance MonadTokenParser m => MonadParser (Layout m) where
  satisfy p   = try $ layoutEq Other *> lift (satisfy p)
  satisfy8 p  = try $ layoutEq Other *> lift (satisfy8 p)
  line        = lift line
  mark        = lift mark
  release     = lift . release
  liftIt      = lift . liftIt
  unexpected  = lift . unexpected
  try         = Layout . try . unlayout
  labels m s  = Layout $ labels (unlayout m) s
  skipMany    = Layout . skipMany . unlayout
  highlight h = Layout . highlight h . unlayout

instance MonadDiagnostic e m => MonadDiagnostic e (Layout m) where
  fatalWith xs r e = lift $ fatalWith xs r e
  errWith xs r e   = lift $ errWith xs r e
  logWith l xs r e = lift $ logWith l xs r e

instance MonadTokenParser m => MonadTokenParser (Layout m) where
  whiteSpace = skipOptional $ try (() <$ layoutEq WhiteSpace <?> "")
  nesting (Layout m) = disableLayout $ Layout (nesting m)
  semi = getLayout layoutStack >>= \ stk -> case stk of
    (DisabledLayout _:_) -> lift semi
    _ -> try (';' <$ layoutEq VirtualSemi <?> "virtual semi-colon")
     <|> lift semi

instance MonadTokenParser m => MonadLayoutParser (Layout m) where
  getLayout l = Layout $ access l
  setLayout l t = () <$ (Layout $ l ~= t)
  modLayout l f = () <$ (Layout $ l %= f)
  layout = do
    bol <- getLayout layoutBol
    m <- mark
    lift whiteSpace
    r <- mark
    if near m r && not bol
      then onside m r
      else do
        stk <- getLayout layoutStack
        case compare (column r) (depth stk) of
          LT -> case stk of
            (IndentedLayout _:xs) -> VirtualRightBrace <$ setLayout layoutStack xs <* setLayout layoutBol True
            [] -> unexpected "empty layout"
            _  -> unexpected "layout"
          EQ -> return VirtualSemi
          GT -> onside m r
    where
      onside m r
        | r /= m    = pure WhiteSpace
        | otherwise = setLayout layoutBol False *> option Other (VirtualRightBrace <$ eof <* trailing)
      trailing = getLayout layoutStack >>= \ stk -> case stk of
          (IndentedLayout _:xs) -> setLayout layoutStack xs
          _ -> empty

      depth []                   = 0
      depth (IndentedLayout r:_) = column r
      depth (DisabledLayout _:_) = -1

instance MonadState s m => MonadState s (Layout m) where
  get = Layout $ lift get
  put = Layout . lift . put

instance MonadReader e m => MonadReader e (Layout m) where
  ask = Layout $ lift ask
  local f (Layout m) = Layout $ local f m

instance MonadWriter w m => MonadWriter w (Layout m) where
  tell = Layout . lift . tell
  listen (Layout m) = Layout $ listen m
  pass (Layout m) = Layout $ pass m

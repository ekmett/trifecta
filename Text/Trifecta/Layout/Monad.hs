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
module Text.Trifecta.Layout.Monad
  ( Layout(..)
  , runLayout
  ) where

import Control.Applicative
import Control.Category
import Control.Monad
import Control.Monad.State.Class
import Control.Monad.Trans.State.Strict (StateT(..))
import Control.Monad.Writer.Class
import Control.Monad.Reader.Class
import Control.Monad.Cont.Class
import Control.Monad.Trans.Class
import Prelude hiding ((.), id)
import Text.Trifecta.Diagnostic.Class
import Text.Trifecta.Parser.Class
import Text.Trifecta.Parser.Mark
import Text.Trifecta.Parser.Combinators
import Text.Trifecta.Layout.Prim
import Text.Trifecta.Layout.Class
import Text.Trifecta.Layout.Combinators
import Text.Trifecta.Rope.Delta

-- | Adds Haskell-style "layout" to base parser
newtype Layout m a = Layout { unlayout :: StateT LayoutState m a }
  deriving (Functor, Applicative, Alternative, Monad, MonadPlus, MonadTrans, MonadCont)

runLayout :: Monad m => Layout m a -> LayoutState -> m (a, LayoutState)
runLayout = runStateT . unlayout

instance MonadParser m => MonadParser (Layout m) where
  satisfy p   = try $ layoutEq Other *> lift (satisfy p)
  satisfy8 p  = try $ layoutEq Other *> lift (satisfy8 p)
  line        = lift line
  unexpected  = lift . unexpected
  try         = Layout . try . unlayout
  labels m s  = Layout $ labels (unlayout m) s
  skipMany    = Layout . skipMany . unlayout
  highlightInterval h s e = lift $ highlightInterval h s e
  someSpace   = try $ (layoutEq WhiteSpace <?> "")
  nesting (Layout m) = disableLayout $ Layout (nesting m)
  semi = getLayout layoutStack >>= \ stk -> case stk of
    (DisabledLayout _:_) -> lift semi
    _ -> try (';' <$ layoutEq VirtualSemi <?> "virtual semi-colon")
     <|> lift semi
  skipping = lift . skipping
  position = lift position
  slicedWith f (Layout m) = Layout $ slicedWith f m
  lookAhead (Layout m) = Layout $ lookAhead m

instance MonadMark d m => MonadMark (LayoutMark d) (Layout m) where
  mark = LayoutMark <$> getLayout id <*> lift mark
  release (LayoutMark s m) = lift (release m) *> setLayout id s

instance MonadDiagnostic e m => MonadDiagnostic e (Layout m) where
  throwDiagnostic = lift . throwDiagnostic
  logDiagnostic = lift . logDiagnostic

instance MonadParser m => MonadLayout (Layout m) where
  layout = buildLayoutParser (lift whiteSpace)
  layoutState f = Layout . StateT $ return . f

buildLayoutParser :: MonadLayout m => m () -> m LayoutToken
buildLayoutParser realWhiteSpace = do
  bol <- getLayout layoutBol
  m <- position
  realWhiteSpace
  r <- position
  if near m r && not bol
    then onside m r
    else getLayout layoutStack >>= \stk -> case compare (column r) (depth stk) of
      GT -> onside m r
      EQ -> return VirtualSemi
      LT -> case stk of
        (IndentedLayout _:xs) -> VirtualRightBrace <$ setLayout layoutStack xs <* setLayout layoutBol True
        _ -> unexpected "layout context"
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

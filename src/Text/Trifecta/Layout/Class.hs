{-# LANGUAGE MultiParamTypeClasses #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Trifecta.Layout.Class
-- Copyright   :  (C) 2011 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
----------------------------------------------------------------------------
module Text.Trifecta.Layout.Class
  ( MonadLayout(..)
  ) where

import Data.Monoid
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Identity
import qualified Control.Monad.Trans.State.Lazy as Lazy
import qualified Control.Monad.Trans.State.Strict as Strict
import qualified Control.Monad.Trans.Writer.Lazy as Lazy
import qualified Control.Monad.Trans.Writer.Strict as Strict
import qualified Control.Monad.Trans.RWS.Lazy as Lazy
import qualified Control.Monad.Trans.RWS.Strict as Strict
import Text.Trifecta.Layout.Prim
import Text.Trifecta.Parser.Class

class MonadParser m => MonadLayout m where
  layout    :: m LayoutToken
  layoutState :: (LayoutState -> (a, LayoutState)) -> m a

instance MonadLayout m => MonadLayout (Strict.StateT s m) where
  layout = lift layout
  layoutState = lift . layoutState

instance MonadLayout m => MonadLayout (Lazy.StateT s m) where
  layout = lift layout
  layoutState = lift . layoutState

instance MonadLayout m => MonadLayout (ReaderT e m) where
  layout = lift layout
  layoutState = lift . layoutState

instance (Monoid w, MonadLayout m) => MonadLayout (Strict.WriterT w m) where
  layout = lift layout
  layoutState = lift . layoutState

instance (Monoid w, MonadLayout m) => MonadLayout (Lazy.WriterT w m) where
  layout = lift layout
  layoutState = lift . layoutState

instance (Monoid w, MonadLayout m) => MonadLayout (Strict.RWST r w s m) where
  layout = lift layout
  layoutState = lift . layoutState

instance (Monoid w, MonadLayout m) => MonadLayout (Lazy.RWST r w s m) where
  layout = lift layout
  layoutState = lift . layoutState

instance MonadLayout m => MonadLayout (IdentityT m) where
  layout = lift layout
  layoutState = lift . layoutState

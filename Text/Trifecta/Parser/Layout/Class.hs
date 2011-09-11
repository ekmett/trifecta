{-# LANGUAGE MultiParamTypeClasses #-}
module Text.Trifecta.Parser.Layout.Class
  ( MonadLayoutParser(..)
  ) where

import Data.Lens.Common
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
import Text.Trifecta.Parser.Layout.Prim
import Text.Trifecta.Parser.Token.Class

class MonadTokenParser m => MonadLayoutParser m where
  layout    :: m LayoutToken
  getLayout :: Lens LayoutState t -> m t
  setLayout :: Lens LayoutState t -> t -> m ()
  modLayout :: Lens LayoutState t -> (t -> t) -> m ()

instance MonadLayoutParser m => MonadLayoutParser (Strict.StateT s m) where
  layout = lift layout
  getLayout l = lift $ getLayout l
  setLayout l t = lift $ setLayout l t
  modLayout l f = lift $ modLayout l f

instance MonadLayoutParser m => MonadLayoutParser (Lazy.StateT s m) where
  layout = lift layout
  getLayout l = lift $ getLayout l
  setLayout l t = lift $ setLayout l t
  modLayout l f = lift $ modLayout l f

instance MonadLayoutParser m => MonadLayoutParser (ReaderT e m) where
  layout = lift layout
  getLayout l = lift $ getLayout l
  setLayout l t = lift $ setLayout l t
  modLayout l f = lift $ modLayout l f

instance (Monoid w, MonadLayoutParser m) => MonadLayoutParser (Strict.WriterT w m) where
  layout = lift layout
  getLayout l = lift $ getLayout l
  setLayout l t = lift $ setLayout l t
  modLayout l f = lift $ modLayout l f

instance (Monoid w, MonadLayoutParser m) => MonadLayoutParser (Lazy.WriterT w m) where
  layout = lift layout
  getLayout l = lift $ getLayout l
  setLayout l t = lift $ setLayout l t
  modLayout l f = lift $ modLayout l f

instance (Monoid w, MonadLayoutParser m) => MonadLayoutParser (Strict.RWST r w s m) where
  layout = lift layout
  getLayout l = lift $ getLayout l
  setLayout l t = lift $ setLayout l t
  modLayout l f = lift $ modLayout l f

instance (Monoid w, MonadLayoutParser m) => MonadLayoutParser (Lazy.RWST r w s m) where
  layout = lift layout
  getLayout l = lift $ getLayout l
  setLayout l t = lift $ setLayout l t
  modLayout l f = lift $ modLayout l f

instance MonadLayoutParser m => MonadLayoutParser (IdentityT m) where
  layout = lift layout
  getLayout l = lift $ getLayout l
  setLayout l t = lift $ setLayout l t
  modLayout l f = lift $ modLayout l f

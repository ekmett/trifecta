{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, UndecidableInstances, FlexibleInstances, FlexibleContexts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Trifecta.Parser.Mark
-- Copyright   :  (c) Edward Kmett 2011
-- License     :  BSD3
--
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable
--
-----------------------------------------------------------------------------
module Text.Trifecta.Parser.Mark
  ( MonadMark(..)
  ) where

import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Lazy as Lazy
import Control.Monad.Trans.State.Strict as Strict
import Control.Monad.Trans.Writer.Lazy as Lazy
import Control.Monad.Trans.Writer.Strict as Strict
import Control.Monad.Trans.RWS.Lazy as Lazy
import Control.Monad.Trans.RWS.Strict as Strict
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Identity
import Data.Functor.Yoneda
import Data.Monoid
import Text.Trifecta.Rope.Delta
import Text.Trifecta.Parser.Class

class (MonadParser m, HasDelta d) => MonadMark d m | m -> d where
  -- | mark the current location so it can be used in constructing a span, or for later seeking
  mark :: m d
  -- | Seek a previously marked location
  release :: d -> m ()

instance MonadMark d m => MonadMark d (Lazy.StateT s m) where
  mark = lift mark
  release = lift . release

instance MonadMark d m => MonadMark d (Strict.StateT s m) where
  mark = lift mark
  release = lift . release

instance MonadMark d m => MonadMark d (ReaderT e m) where
  mark = lift mark
  release = lift . release

instance (MonadMark d m, Monoid w) => MonadMark d (Strict.WriterT w m) where
  mark = lift mark
  release = lift . release

instance (MonadMark d m, Monoid w) => MonadMark d (Lazy.WriterT w m) where
  mark = lift mark
  release = lift . release

instance (MonadMark d m, Monoid w) => MonadMark d (Lazy.RWST r w s m) where
  mark = lift mark
  release = lift . release

instance (MonadMark d m, Monoid w) => MonadMark d (Strict.RWST r w s m) where
  mark = lift mark
  release = lift . release

instance MonadMark d m => MonadMark d (IdentityT m) where
  mark = lift mark
  release = lift . release

instance MonadMark d m => MonadMark d (Yoneda m) where
  mark = lift mark
  release = lift . release

-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Trifecta.Language.Class
-- Copyright   :  (C) 2011 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
----------------------------------------------------------------------------
module Text.Trifecta.Language.Class
  ( MonadLanguage(..)
  , asksLanguage
  ) where

import Control.Monad (liftM)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Identity
import qualified Control.Monad.Trans.Writer.Strict as Strict
import qualified Control.Monad.Trans.State.Strict as Strict
import qualified Control.Monad.Trans.RWS.Strict as Strict
import qualified Control.Monad.Trans.Writer.Lazy as Lazy
import qualified Control.Monad.Trans.State.Lazy as Lazy
import qualified Control.Monad.Trans.RWS.Lazy as Lazy
import Data.Monoid
import Text.Trifecta.Language.Prim
import Text.Trifecta.Parser.Class

class MonadParser m => MonadLanguage m where
  askLanguage :: m (LanguageDef m)

asksLanguage :: MonadLanguage m => (LanguageDef m -> r) -> m r
asksLanguage f = liftM f askLanguage

instance MonadLanguage m => MonadLanguage (Strict.StateT s m) where
  askLanguage = liftM liftLanguageDef $ lift askLanguage

instance MonadLanguage m => MonadLanguage (Lazy.StateT s m) where
  askLanguage = liftM liftLanguageDef $ lift askLanguage

instance (Monoid w, MonadLanguage m) => MonadLanguage (Strict.WriterT w m) where
  askLanguage = liftM liftLanguageDef $ lift askLanguage

instance (Monoid w, MonadLanguage m) => MonadLanguage (Lazy.WriterT w m) where
  askLanguage = liftM liftLanguageDef $ lift askLanguage

instance MonadLanguage m => MonadLanguage (ReaderT s m) where
  askLanguage = liftM liftLanguageDef $ lift askLanguage

instance MonadLanguage m => MonadLanguage (IdentityT m) where
  askLanguage = liftM liftLanguageDef $ lift askLanguage

instance (Monoid w, MonadLanguage m) => MonadLanguage (Strict.RWST r w s m) where
  askLanguage = liftM liftLanguageDef $ lift askLanguage

instance (Monoid w, MonadLanguage m) => MonadLanguage (Lazy.RWST r w s m) where
  askLanguage = liftM liftLanguageDef $ lift askLanguage

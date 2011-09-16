module Text.Trifecta.Parser.Literate.Class
  ( MonadLiterate(..)
  ) where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Identity
import qualified Control.Monad.Trans.State.Strict as Strict
import qualified Control.Monad.Trans.State.Lazy as Lazy
import qualified Control.Monad.Trans.Writer.Strict as Strict
import qualified Control.Monad.Trans.Writer.Lazy as Lazy
import qualified Control.Monad.Trans.RWS.Strict as Strict
import qualified Control.Monad.Trans.RWS.Lazy as Lazy
import Data.Monoid
import Text.Trifecta.Parser.Class
import Text.Trifecta.Parser.Literate.Prim

class MonadParser m => MonadLiterate m where
  literateState :: (LiterateState -> (a, LiterateState)) -> m a

instance MonadLiterate m => MonadLiterate (Strict.StateT s m) where
  literateState = lift . literateState

instance MonadLiterate m => MonadLiterate (Lazy.StateT s m) where
  literateState = lift . literateState

instance (Monoid w, MonadLiterate m) => MonadLiterate (Strict.WriterT w m) where
  literateState = lift . literateState

instance (Monoid w, MonadLiterate m) => MonadLiterate (Lazy.WriterT w m) where
  literateState = lift . literateState

instance (Monoid w, MonadLiterate m) => MonadLiterate (Strict.RWST r w s m) where
  literateState = lift . literateState

instance (Monoid w, MonadLiterate m) => MonadLiterate (Lazy.RWST r w s m) where
  literateState = lift . literateState

instance MonadLiterate m => MonadLiterate (IdentityT m) where
  literateState = lift . literateState

instance MonadLiterate m => MonadLiterate (ReaderT e m) where
  literateState = lift . literateState

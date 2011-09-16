module Text.Trifecta.Parser.Literate.Class
  ( Literate(..)
  ) where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Identity
import qualified Control.Monad.Trans.StateT.Strict as Strict
import qualified Control.Monad.Trans.StateT.Lazy as Lazy
import qualified Control.Monad.Trans.WriterT.Strict as Strict
import qualified Control.Monad.Trans.WriterT.Lazy as Lazy
import qualified Control.Monad.Trans.RWST.Strict as Strict
import qualified Control.Monad.Trans.RWST.Lazy as Lazy
import Text.Trifecta.Parser.Literate.Prim

-- | Add literate comment support to a base monad
class MonadLiterate m where
  -- TODO: add IntMap LiterateState -- to keep track of marks!
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

instance (MonadLiterate m) => MonadLiterate (IdentityT m) where
  literateState = lift . literateState

instance (MonadLiterate m) => MonadLiterate (ReaderT m) where
  literateState = lift . literateState

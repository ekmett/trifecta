module Text.Trifecta.Highlighter.Class 
  ( MonadHighlighter(..)
  ) where

import Data.IntervalMap.FingerTree
import Data.Monoid
import Text.Trifecta.Rope.Delta
import Text.Trifecta.Parser.Token.Highlight
import qualified Control.Monad.Trans.State.Lazy as Lazy
import qualified Control.Monad.Trans.State.Strict as Strict
import qualified Control.Monad.Trans.RWS.Lazy as Lazy
import qualified Control.Monad.Trans.RWS.Strict as Strict
import qualified Control.Monad.Trans.Writer.Lazy as Lazy
import qualified Control.Monad.Trans.Writer.Strict as Strict
import Control.Monad.Trans.Class
import Control.Monad.Trans.Error
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader

class Monad m => MonadHighlighter m where
  highlights :: m (IntervalMap Delta TokenHighlight)

instance MonadHighlighter m => MonadHighlighter (Lazy.StateT s m) where
  highlights = lift highlights

instance MonadHighlighter m => MonadHighlighter (Strict.StateT s m) where
  highlights = lift highlights

instance (MonadHighlighter m, Monoid w) => MonadHighlighter (Lazy.WriterT w m) where
  highlights = lift highlights

instance (MonadHighlighter m, Monoid w) => MonadHighlighter (Strict.WriterT w m) where
  highlights = lift highlights

instance MonadHighlighter m => MonadHighlighter (ReaderT e m) where
  highlights = lift highlights

instance (MonadHighlighter m, Monoid w) => MonadHighlighter (Lazy.RWST r w s m) where
  highlights = lift highlights

instance (MonadHighlighter m, Monoid w) => MonadHighlighter (Strict.RWST r w s m) where
  highlights = lift highlights

instance MonadHighlighter m => MonadHighlighter (MaybeT m) where
  highlights = lift highlights

instance (MonadHighlighter m, Error e) => MonadHighlighter (ErrorT e m) where
  highlights = lift highlights

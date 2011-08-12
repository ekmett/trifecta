{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FunctionalDependencies, FlexibleContexts, UndecidableInstances #-}
module Text.Trifecta.Diagnostic.Class
  ( MonadDiagnostic(..)
  ) where

import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Lazy as Lazy
import Control.Monad.Trans.State.Strict as Strict
import Control.Monad.Trans.RWS.Lazy as Lazy
import Control.Monad.Trans.RWS.Strict as Strict
import Control.Monad.Trans.Writer.Lazy as Lazy
import Control.Monad.Trans.Writer.Strict as Strict
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Identity
import Data.Monoid
import Text.Trifecta.Diagnostic.Prim
import Text.Trifecta.Diagnostic.Level
import Text.Trifecta.Diagnostic.Rendering.Prim

class Monad m => MonadDiagnostic e m | m -> e where
  fatalWith ::                    [Diagnostic e] -> [Rendering] -> e -> m a  -- consuming error
  errWith   ::                    [Diagnostic e] -> [Rendering] -> e -> m a  -- non-consuming error, handled by <|> 
  logWith   :: DiagnosticLevel -> [Diagnostic e] -> [Rendering] -> e -> m () -- log an error and continue

instance MonadDiagnostic e m => MonadDiagnostic e (Lazy.StateT s m) where
  fatalWith d r e = lift (fatalWith d r e)
  errWith   d r e = lift (errWith d r e)
  logWith l d r e = lift (logWith l d r e)

instance MonadDiagnostic e m => MonadDiagnostic e (Strict.StateT s m) where
  fatalWith d r e = lift (fatalWith d r e)
  errWith   d r e = lift (errWith d r e)
  logWith l d r e = lift (logWith l d r e)

instance MonadDiagnostic e m => MonadDiagnostic e (ReaderT r m) where
  fatalWith d r e = lift (fatalWith d r e)
  errWith   d r e = lift (errWith d r e)
  logWith l d r e = lift (logWith l d r e)

instance (MonadDiagnostic e m, Monoid w) => MonadDiagnostic e (Lazy.WriterT w m) where
  fatalWith d r e = lift (fatalWith d r e)
  errWith   d r e = lift (errWith d r e)
  logWith l d r e = lift (logWith l d r e)

instance (MonadDiagnostic e m, Monoid w) => MonadDiagnostic e (Strict.WriterT w m) where
  fatalWith d r e = lift (fatalWith d r e)
  errWith   d r e = lift (errWith d r e)
  logWith l d r e = lift (logWith l d r e)

instance (MonadDiagnostic e m, Monoid w) => MonadDiagnostic e (Lazy.RWST r w s m) where
  fatalWith d r e = lift (fatalWith d r e)
  errWith   d r e = lift (errWith d r e)
  logWith l d r e = lift (logWith l d r e)

instance (MonadDiagnostic e m, Monoid w) => MonadDiagnostic e (Strict.RWST r w s m) where
  fatalWith d r e = lift (fatalWith d r e)
  errWith   d r e = lift (errWith d r e)
  logWith l d r e = lift (logWith l d r e)

instance MonadDiagnostic e m => MonadDiagnostic e (IdentityT m) where
  fatalWith d r e = lift (fatalWith d r e)
  errWith   d r e = lift (errWith d r e)
  logWith l d r e = lift (logWith l d r e)

{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FunctionalDependencies, FlexibleContexts, UndecidableInstances #-}
module Text.Trifecta.Diagnostic.Class
  ( MonadDiagnostic(..)
  ) where

import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Lazy as Lazy
import Control.Monad.Trans.State.Strict as Strict
import Text.Trifecta.Diagnostic.Prim

class Monad m => MonadDiagnostic e m | m -> e where
  record  :: Diagnostic e -> m ()
  fatal   :: e -> m a -- consuming error
  err     :: e -> m a -- non-consuming error
  warn    :: e -> m ()
  note    :: e -> m ()
  verbose :: Int -> e -> m ()

instance MonadDiagnostic e m => MonadDiagnostic e (Lazy.StateT s m) where
  record    = lift . record
  fatal     = lift . fatal
  err       = lift . err
  warn      = lift . warn
  note      = lift . note
  verbose n = lift . verbose n

instance MonadDiagnostic e m => MonadDiagnostic e (Strict.StateT s m) where
  record    = lift . record
  fatal     = lift . fatal
  err       = lift . err
  warn      = lift . warn
  note      = lift . note
  verbose n = lift . verbose n

-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Trifecta.Diagnostic.Combinators
-- Copyright   :  (C) 2011 Edward Kmett,
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Combinators for throwing and logging expressive diagnostics
----------------------------------------------------------------------------
module Text.Trifecta.Diagnostic.Combinators
  ( panic, panicAt
  , fatal, fatalAt
  , err, errAt
  , warn, warnAt
  , note, noteAt
  , verbose, verboseAt
  ) where

import Control.Applicative
import Control.Monad.Instances ()
import Text.Trifecta.Parser.Class
import Text.Trifecta.Diagnostic.Class
import Text.Trifecta.Diagnostic.Prim
import Text.Trifecta.Diagnostic.Level
import Text.Trifecta.Diagnostic.Rendering.Prim
import Text.Trifecta.Diagnostic.Rendering.Caret
import Text.Trifecta.Rope.Delta

rendCaret :: MonadParser m => m Rendering
rendCaret = (delta >>= addCaret) <$> rend

panicAt, fatalAt, errAt :: MonadDiagnostic e m => [Diagnostic e] -> e -> Rendering -> m a
panicAt es e r = throwDiagnostic $ Diagnostic (Right r) Panic e es
fatalAt es e r = throwDiagnostic $ Diagnostic (Right r) Fatal e es
errAt   es e r = throwDiagnostic $ Diagnostic (Right r) Error e es

panic, fatal, err :: (MonadParser m, MonadDiagnostic e m) => [Diagnostic e] -> e -> m a
panic es e = rendCaret >>= panicAt es e
fatal es e = rendCaret >>= fatalAt es e
err es e   = rendCaret >>= errAt es e

warnAt, noteAt :: MonadDiagnostic e m => [Diagnostic e] -> e -> Rendering -> m ()
warnAt es e r = logDiagnostic $ Diagnostic (Right r) Warning e es
noteAt es e r = logDiagnostic $ Diagnostic (Right r) Note e es

verboseAt :: MonadDiagnostic e m => Int -> [Diagnostic e] -> e -> Rendering -> m ()
verboseAt l es e r = logDiagnostic $ Diagnostic (Right r) (Verbose l) e es

warn, note :: (MonadParser m, MonadDiagnostic e m) => [Diagnostic e] -> e -> m ()
warn es e = rendCaret >>= warnAt es e
note es e = rendCaret >>= noteAt es e

verbose :: (MonadParser m, MonadDiagnostic e m) => Int -> [Diagnostic e] -> e -> m ()
verbose l es e = rendCaret >>= verboseAt l es e

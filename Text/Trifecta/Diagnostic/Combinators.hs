module Text.Trifecta.Diagnostic.Combinators 
  ( fatal
  , err
  , warn
  , note
  , verbose
  , warnWith
  , noteWith
  , verboseWith
  ) where

import Text.Trifecta.Diagnostic.Class
import Text.Trifecta.Diagnostic.Prim
import Text.Trifecta.Diagnostic.Level
import Text.Trifecta.Diagnostic.Rendering.Prim

fatal :: MonadDiagnostic e m => e -> m a
fatal = fatalWith [] []

err :: MonadDiagnostic e m => e -> m a
err = errWith [] []

warn :: MonadDiagnostic e m => e -> m ()
warn = warnWith [] []

note :: MonadDiagnostic e m => e -> m ()
note = noteWith [] []

verbose :: MonadDiagnostic e m => Int -> e -> m ()
verbose n = verboseWith n [] []

warnWith :: MonadDiagnostic e m => [Diagnostic e] -> [Rendering] -> e -> m ()
warnWith = logWith Warning

noteWith :: MonadDiagnostic e m => [Diagnostic e] -> [Rendering] -> e -> m ()
noteWith = logWith Note

verboseWith :: MonadDiagnostic e m => Int -> [Diagnostic e] -> [Rendering] -> e -> m ()
verboseWith n = logWith (Verbose n)

-- sublimate :: MonadDiagnostic e m => m a -> m a
-- sublimate a = a `catchError` \e -> 
--   if fatalErr e then throwError e else do 
--     m <- here
--     tell (diagnose m e)

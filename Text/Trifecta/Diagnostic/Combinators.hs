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
 
fatal :: MonadDiagnostic e m => e -> m a
fatal = fatalWith [] []

err :: MonadDiagnostic e m => e -> m a
err = errWith [] []

warn :: MonadDiagnostic e m => e -> m ()
warn = warnWith [] []

note :: MonadDiagnostic e m => e -> m ()
note = noteWith [] []

verbose :: MonadDiagnostic e m => Int -> e -> m ()
verbose n = verboseWith [] []

warnWith :: MonadDiagnostic e m => [Diagnostic] -> [Rendering] -> e -> m ()
warnWith = logWith Warning

noteWith :: MonadDiagnostic e m => [Diagnostic] -> [Rendering] -> e -> m ()
noteWith = logWith Note

verboseWith :: MonadDiagnostic e m => [Diagnostic] -> [Rendering] -> e -> m ()
verboseWith n = logWith (Verbose n)

-- sublimate :: MonadDiagnostic e m => m a -> m a
-- sublimate a = a `catchError` \e -> 
--   if fatalErr e then throwError e else do 
--     m <- here
--     tell (diagnose m e)

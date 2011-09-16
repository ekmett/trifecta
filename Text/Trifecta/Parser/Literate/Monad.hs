module Text.Trifecta.Parser.Literate.Monad
  ( Literate(..)
  ) where

import Control.Monad
import Control.Monad.Trans.State.Strict
import Control.Monad.State.Class
import Control.Monad.Cont.Class
import Control.Monad.Reader.Class
import Control.Monad.Writer.Class
import Control.Monad.Diagnostic.Class
import Text.Trifecta.Parser.Class
import Text.Trifecta.Parser.Mark
import Text.Trifecta.Parser.Literate.Prim
import Text.Trifecta.Parser.Language.Class
import Text.Trifecta.Parser.Literate.Prim
import Text.Trifecta.Parser.Literate.Class
import Text.Trifecta.Parser.Literate.Combinators

newtype Literate m a = Literate { runLiterate :: StateT LiterateState m a }
  deriving (Functor,Applicative,Alternative,Monad,MonadPlus,MonadTrans,MonadCont)

instance MonadParser m => MonadParser (Literate m) where
  someSpace                 = someLiterateSpace
  highlightInterval h s e   = lift $ highlightInterval h s e
  try (Literate m)          = Literate $ try m
  nesting (Literate m)      = Literate $ nesting m
  skipMany (Literate m)     = Literate $ skipMany m
  slicedWith f (Literate m) = Literate $ slicedWith f m
  labels (Literate m) p     = Literate $ labels m p
  semi       = lift semi
  line       = lift line
  satisfy    = lift . satisfy
  satisfy8   = lift . satisfy8
  unexpected = lift . unexpected
  liftIt     = lift . liftIt

instance MonadMark d m => MonadMark (LiterateMark d) (Literate m) where
  mark = LiterateMark <$> getLiterateState <*> lift mark
  release (LiterateMark s d) = do
    lift $ release d
    setLiterateState s

instance MonadLanguage m => MonadLanguage (Literate m) where
  askLanguage = liftM liftLanguage $ lift askLanguage

instance MonadDiagnostic m => MonadDiagnostic (Literate m) where
  fatalWith xs rs e = lift $ fatalWith xs rs e
  errWith xs rs e = lift $ errWith xs rs e
  logWith l xs rs e = lift $ logWith l xs rs e

instance MonadState s m => MonadState s (Literate m) where
  get = lift get
  put = lift . put

instance MonadReader e m => MonadReader e (Literate m) where
  ask = lift ask
  local f (Literate m) = Literate $ local f m

instance MonadWriter w m => MonadWriter w (Literate m) where
  tell = lift . tell
  listen (Literate m) = Literate $ listen m
  pass (Literate m)   = Literate $ pass m

{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, UndecidableInstances, GeneralizedNewtypeDeriving #-}
module Text.Trifecta.Parser.Language.Monad
  ( Language(..)
  , runLanguage
  ) where

import Control.Applicative
import Control.Monad ()
import Control.Monad.Trans.Class
import Control.Monad.Reader
import Control.Monad.Writer.Class
import Control.Monad.State.Class
import Control.Monad.Cont.Class
import Text.Trifecta.Diagnostic.Class
import Text.Trifecta.Parser.Class
import Text.Trifecta.Parser.Mark
import Text.Trifecta.Parser.Token.Style
import Text.Trifecta.Parser.Language.Def
import Text.Trifecta.Parser.Language.Class

newtype Language m a = Language { unlanguage :: ReaderT (LanguageDef (Language m)) m a }
  deriving (Functor,Applicative,Alternative,Monad,MonadPlus,MonadCont)

runLanguage :: Language m a -> LanguageDef (Language m) -> m a
runLanguage = runReaderT . unlanguage

instance MonadParser m => MonadLanguage (Language m) where
  askLanguage = Language ask

instance MonadTrans Language where
  lift = Language . lift

instance MonadParser m => MonadParser (Language m) where
  highlightInterval h s e = lift $ highlightInterval h s e
  someSpace = asksLanguage languageCommentStyle >>= buildSomeSpaceParser (lift someSpace)
  nesting (Language (ReaderT m)) = Language $ ReaderT $ nesting . m
  semi = lift semi
  try (Language m) = Language $ try m
  labels (Language m) ss = Language $ labels m ss
  satisfy = lift . satisfy
  satisfy8 = lift . satisfy8
  skipping = lift . skipping
  unexpected = lift . unexpected
  position = lift position
  line = lift line
  lookAhead (Language m) = Language (lookAhead m)
  slicedWith f (Language m) = Language $ ReaderT $ slicedWith f . runReaderT m

instance MonadMark d m => MonadMark d (Language m) where
  mark = lift mark
  release = lift . release

instance MonadDiagnostic e m => MonadDiagnostic e (Language m) where
  fatalWith xs rs e = lift $ fatalWith xs rs e
  errWith xs rs e = lift $ errWith xs rs e
  logWith l xs rs e = lift $ logWith l xs rs e

instance MonadState s m => MonadState s (Language m) where
  get = Language $ lift get
  put s = Language $ lift $ put s

instance MonadWriter w m => MonadWriter w (Language m) where
  tell = Language . lift . tell
  pass = Language . pass . unlanguage
  listen = Language . listen . unlanguage

instance MonadReader e m => MonadReader e (Language m) where
  ask = Language $ lift ask
  local f (Language m) = Language $ ReaderT $ \e -> local f (runReaderT m e)

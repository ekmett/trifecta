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
import Text.Trifecta.Parser.Token.Class
import Text.Trifecta.Parser.Token.Combinators
import Text.Trifecta.Parser.Token.Style
import Text.Trifecta.Parser.Language.Def
import Text.Trifecta.Parser.Language.Class

newtype Language m a = Language { unlanguage :: ReaderT (LanguageDef (Language m)) m a }
  deriving (Functor,Applicative,Alternative,Monad,MonadPlus,MonadParser,MonadCont)

runLanguage :: Language m a -> LanguageDef (Language m) -> m a
runLanguage = runReaderT . unlanguage

instance MonadParser m => MonadLanguage (Language m) where
  askLanguage = Language ask

instance MonadTrans Language where
  lift = Language . lift

instance MonadParser m => MonadTokenParser (Language m) where
  whiteSpace = asksLanguage languageCommentStyle >>= buildWhiteSpaceParser
  nesting = id
  semi = symbolic ';'

instance MonadDiagnostic e m => MonadDiagnostic e (Language m) where
  fatalWith xs rs e = lift $ fatalWith xs rs e
  errWith xs rs e = lift $ errWith xs rs e
  logWith l xs rs e = lift $ logWith l xs rs e

instance MonadState s m => MonadState s (Language m) where
  get = Language $ lift get
  put = Language . lift . put

instance MonadWriter w m => MonadWriter w (Language m) where
  tell = Language . lift . tell
  pass = Language . pass . unlanguage
  listen = Language . listen . unlanguage

instance MonadReader e m => MonadReader e (Language m) where
  ask = Language $ lift ask
  local f (Language m) = Language $ ReaderT $ \e -> local f (runReaderT m e)

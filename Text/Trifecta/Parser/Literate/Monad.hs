{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, UndecidableInstances, GeneralizedNewtypeDeriving #-}
module Text.Trifecta.Parser.Literate.Monad
  ( Literate(..)
  , runLiterate
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.State.Strict (StateT(..))
import Control.Monad.Trans.Class
import Control.Monad.State.Class
import Control.Monad.Cont.Class
import Control.Monad.Reader.Class
import Control.Monad.Writer.Class
import Data.Semigroup
import Text.Trifecta.Diagnostic.Class
import Text.Trifecta.Parser.Char
import Text.Trifecta.Parser.Class
import Text.Trifecta.Parser.Combinators
import Text.Trifecta.Parser.Mark
import Text.Trifecta.Parser.Language.Prim
import Text.Trifecta.Parser.Language.Class
import Text.Trifecta.Parser.Literate.Prim
import Text.Trifecta.Parser.Literate.Class
import Text.Trifecta.Parser.Literate.Combinators
import Text.Trifecta.Rope.Delta

newtype Literate m a = Literate { unliterate :: StateT LiterateState m a }
  deriving (Functor,Applicative,Alternative,Monad,MonadPlus,MonadTrans,MonadCont)

runLiterate :: Monad m => Literate m a -> LiterateState -> m (a, LiterateState)
runLiterate = runStateT . unliterate

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
  position = lift position
  skipping d
    | near (Columns 0 0) d = lift $ skipping d -- we don't change literate states within a line
    | otherwise = do
      s <- position
      let e = s <> d
      () <$ (manyTill (someSpace <|> () <$ anyChar) $ position >>= \p -> guard (e <= p))
  lookAhead (Literate (StateT p)) = Literate $ StateT $ \s -> do
    as' <- lookAhead $ p s
    return (fst as', s)

instance MonadParser m => MonadLiterate (Literate m) where
  literateState f = Literate $ StateT $ return . f

instance MonadMark d m => MonadMark (LiterateMark d) (Literate m) where
  mark = LiterateMark <$> getLiterate <*> lift mark
  release (LiterateMark s d) = lift (release d) *> putLiterate s

instance MonadLanguage m => MonadLanguage (Literate m) where
  askLanguage = liftM liftLanguageDef $ lift askLanguage

instance MonadDiagnostic e m => MonadDiagnostic e (Literate m) where
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

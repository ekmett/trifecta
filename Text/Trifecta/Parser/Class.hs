{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, UndecidableInstances, FlexibleInstances, FlexibleContexts #-}
module Text.Trifecta.Parser.Class 
  ( MonadParser(..)
  , rest
  , (<?>)
  , sliced
  ) where

import Control.Applicative
import Control.Monad (MonadPlus(..))
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Lazy
import Data.ByteString as Strict
import Data.Semigroup
import Data.Set as Set
import Text.Trifecta.Delta
import Text.Trifecta.Rope
import Text.Trifecta.Parser.It

class ( Alternative m, MonadPlus m) => MonadParser m where

  -- * non-committal actions
  try        :: m a -> m a
  labels     :: m a -> Set String -> m a
  liftIt     :: It Rope a -> m a
  mark       :: m Delta
  unexpected :: MonadParser m => String -> m a
  line       :: m ByteString

  -- * actions that commit
  satisfy    :: (Char -> Bool) -> m Char
  release    :: Delta -> m ()

  satisfyAscii :: (Char -> Bool) -> m Char
  satisfyAscii f = toEnum . fromEnum <$> satisfy (f . toEnum . fromEnum)

  skipping :: HasDelta d => d -> m d
  skipping d = do
    m <- mark
    d <$ release (m <> delta d)

instance MonadParser m => MonadParser (StateT s m) where
  satisfy = lift . satisfy
  try (StateT m) = StateT $ try . m
  labels (StateT m) ss = StateT $ \s -> labels (m s) ss
  line = lift line
  liftIt = lift . liftIt
  mark = lift mark 
  release = lift . release
  unexpected = lift . unexpected
  satisfyAscii = lift . satisfyAscii

rest :: MonadParser m => m ByteString
rest = do
  m <- mark
  Strict.drop (columnByte m) <$> line

(<?>) :: MonadParser m => m a -> String -> m a
p <?> msg = labels p (Set.singleton msg)

sliced :: MonadParser m => (a -> Strict.ByteString -> r) -> m a -> m r
sliced f pa = do
  m <- mark
  a <- pa
  r <- mark
  liftIt $ f a <$> sliceIt m r

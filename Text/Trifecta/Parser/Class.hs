{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, UndecidableInstances, FlexibleInstances, FlexibleContexts #-}
module Text.Trifecta.Parser.Class 
  ( MonadParser(..)
  , rest
  , (<?>)
  , log
  , sliced
  ) where

import Control.Applicative
import Control.Monad (MonadPlus(..))
import Text.Trifecta.Diagnostic
import Data.ByteString

class ( Alternative m, MonadPlus m) => MonadParser m where
  satisfy :: (Char -> Bool) -> m Char
  commit :: m a -> m a
  labels :: m a -> Set String -> m a
  it :: It a -> m a
  mark :: m Delta
  release :: Delta -> m ()
  slice :: Delta -> Delta -> m ()
  unexpected :: MonadParser m => String -> m a
  line :: m ByteString

  satisfyAscii :: (Char -> Bool) -> m Char
  satisfyAscii f = toEnum . fromEnum <$> satisfy (f . toEnum . fromEnum)

  skipping :: HasDelta d => d -> m d
  skipping d = do
    m <- mark
    release (m <> d)

instance MonadParser m => MonadParser (StateT s m) where
  satisfy = lift . satisfy
  commit (StateT m) = StateT $ commit . m
  labels (StateT m) ss = StateT $ \s -> labels (m s) ss
  it = lift . liftIt
  mark = lift mark 
  release = lift . release
  unexpected = lift . unexpected
  satisfyAscii = lift . satisfyAscii

rest :: MonadParser m => m ByteString
rest = do
  m <- mark
  drop (columnBytes m) <$> line

(<?>) :: MonadParser m => m a -> String -> m a
p <?> msg = labels p (Set.singleton msg)

log :: MonadError (Seq (Diagnostic e)) m => Diagnostic e -> m ()
log = tell . Seq.singleton

sliced :: MonadParser m => (a -> Strict.ByteString -> r) -> m a -> m r
sliced f pa = do
  m <- mark
  a <- pa
  r <- mark
  it $ f a <$> sliceIt m r

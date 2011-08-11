{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, UndecidableInstances, FlexibleInstances, FlexibleContexts #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Trifecta.Parser.Class
-- Copyright   :  (c) Edward Kmett 2011
-- License     :  BSD3
-- 
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable
-- 
-----------------------------------------------------------------------------
module Text.Trifecta.Parser.Class 
  ( MonadParser(..)
  , restOfLine
  , skipping
  , (<?>)
  , slicedWith
  , sliced
  ) where

import Control.Applicative
import Control.Monad (MonadPlus(..))
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Lazy as Lazy
import Control.Monad.Trans.State.Strict as Strict
import Data.ByteString as Strict
import Data.Semigroup
import Data.Set as Set
import Text.Trifecta.Rope.Delta
import Text.Trifecta.Rope.Prim
import Text.Trifecta.Parser.It

infix 0 <?>

class ( Alternative m, MonadPlus m) => MonadParser m where
  -- non-committal actions
  try        :: m a -> m a
  labels     :: m a -> Set String -> m a
  liftIt     :: It Rope a -> m a
  mark       :: m Delta
  unexpected :: MonadParser m => String -> m a
  line       :: m ByteString

  -- actions that definitely commit
  release    :: Delta -> m ()
  satisfy    :: (Char -> Bool) -> m Char

  satisfyAscii :: (Char -> Bool) -> m Char
  satisfyAscii f = toEnum . fromEnum <$> satisfy (f . toEnum . fromEnum)

instance MonadParser m => MonadParser (Lazy.StateT s m) where
  satisfy = lift . satisfy
  try (Lazy.StateT m) = Lazy.StateT $ try . m
  labels (Lazy.StateT m) ss = Lazy.StateT $ \s -> labels (m s) ss
  line = lift line
  liftIt = lift . liftIt
  mark = lift mark 
  release = lift . release
  unexpected = lift . unexpected
  satisfyAscii = lift . satisfyAscii

instance MonadParser m => MonadParser (Strict.StateT s m) where
  satisfy = lift . satisfy
  try (Strict.StateT m) = Strict.StateT $ try . m
  labels (Strict.StateT m) ss = Strict.StateT $ \s -> labels (m s) ss
  line = lift line
  liftIt = lift . liftIt
  mark = lift mark 
  release = lift . release
  unexpected = lift . unexpected
  satisfyAscii = lift . satisfyAscii

-- useful when we've just recognized something out of band using access to the current line 
skipping :: (MonadParser m, HasDelta d) => d -> m d
skipping d = do
  m <- mark
  d <$ release (m <> delta d)

-- | grab the remainder of the current line
restOfLine :: MonadParser m => m ByteString
restOfLine = do
  m <- mark
  Strict.drop (columnByte m) <$> line

-- | label a parser with a name
(<?>) :: MonadParser m => m a -> String -> m a
p <?> msg = labels p (Set.singleton msg)

-- | run a parser, grabbing all of the text between its start and end points
slicedWith :: MonadParser m => (a -> Strict.ByteString -> r) -> m a -> m r
slicedWith f pa = do
  m <- mark
  a <- pa
  r <- mark
  liftIt $ f a <$> sliceIt m r

-- | run a parser, grabbing all of the text between its start and end points and discarding the original result
sliced :: MonadParser m => m a -> m ByteString
sliced = slicedWith (\_ bs -> bs)
  

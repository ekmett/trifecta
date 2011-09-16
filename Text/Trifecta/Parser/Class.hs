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
  , satisfyAscii
  , restOfLine
  , (<?>)
  , sliced
  , rend
  , whiteSpace
  , highlight
  ) where

import Control.Applicative
import Control.Monad (MonadPlus(..))
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Lazy as Lazy
import Control.Monad.Trans.State.Strict as Strict
import Control.Monad.Trans.Writer.Lazy as Lazy
import Control.Monad.Trans.Writer.Strict as Strict
import Control.Monad.Trans.RWS.Lazy as Lazy
import Control.Monad.Trans.RWS.Strict as Strict
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Identity
import Data.Functor.Yoneda
import Data.Word
import Data.ByteString as Strict
import Data.Char (isSpace)
import Data.ByteString.Internal (w2c)
import Data.Semigroup
import Text.Trifecta.Rope.Delta
import Text.Trifecta.Highlight.Prim
import Text.Trifecta.Diagnostic.Rendering.Prim

infix 0 <?>

class (Alternative m, MonadPlus m) => MonadParser m where
  -- | Take a parser that may consume input, and on failure, go back to where we started and fail as if we didn't consume input.
  try :: m a -> m a

  -- Used to implement (<?>), runs the parser then sets the 'expected' tokens to the list supplied
  labels :: m a -> [String] -> m a

  -- | A version of many that discards its input. Specialized because it can often be implemented more cheaply.
  skipMany :: m a -> m ()
  skipMany p = () <$ many p

  -- | Parse a single character of the input, with UTF-8 decoding
  satisfy :: (Char -> Bool) -> m Char

  -- | Parse a single byte of the input, without UTF-8 decoding
  satisfy8 :: (Word8 -> Bool) -> m Word8

  -- | Usually, someSpace consists of /one/ or more occurrences of a 'space'.
  -- Some parsers may choose to recognize line comments or block (multi line)
  -- comments as white space as well.
  someSpace :: m ()
  someSpace = space *> skipMany space
    where space = satisfy isSpace

  -- | Called when we enter a nested pair of symbols.
  -- Overloadable to disable layout or highlight nested contexts.
  nesting :: m a -> m a
  nesting = id

  -- | Lexeme parser |semi| parses the character \';\' and skips any
  -- trailing white space. Returns the character \';\'.
  semi :: m Char
  semi = satisfyAscii (';'==) <?> ";"

  -- | Used to emit an error on an unexpected token
  unexpected :: String -> m a

  -- | Retrieve the contents of the current line (from the beginning of the line)
  line :: m ByteString

  skipping :: Delta -> m ()

  -- | @highlightInterval@ is called internally in the token parsers.
  -- It delimits ranges of the input recognized by certain parsers that
  -- are useful for syntax highlighting. An interested monad could
  -- choose to listen to these events and construct an interval tree
  -- for later pretty printing purposes.
  highlightInterval :: Highlight -> Delta -> Delta -> m ()
  highlightInterval _ _ _ = pure ()

  position :: m Delta

  -- | run a parser, grabbing all of the text between its start and end points
  slicedWith :: (a -> Strict.ByteString -> r) -> m a -> m r

  -- | @lookAhead p@ parses @p@ without consuming any input.
  lookAhead :: m a -> m a


instance MonadParser m => MonadParser (Lazy.StateT s m) where
  try (Lazy.StateT m) = Lazy.StateT $ try . m
  labels (Lazy.StateT m) ss = Lazy.StateT $ \s -> labels (m s) ss
  line = lift line
  unexpected = lift . unexpected
  satisfy = lift . satisfy
  satisfy8 = lift . satisfy8
  someSpace = lift someSpace
  semi = lift semi
  highlightInterval h s e  = lift $ highlightInterval h s e
  nesting (Lazy.StateT m) = Lazy.StateT $ nesting . m
  skipping = lift . skipping
  position = lift position
  slicedWith f (Lazy.StateT m) = Lazy.StateT $ \s -> slicedWith (\(a,s') b -> (f a b, s')) $ m s
  lookAhead (Lazy.StateT m) = Lazy.StateT $ lookAhead . m

instance MonadParser m => MonadParser (Strict.StateT s m) where
  try (Strict.StateT m) = Strict.StateT $ try . m
  labels (Strict.StateT m) ss = Strict.StateT $ \s -> labels (m s) ss
  line = lift line
  unexpected = lift . unexpected
  satisfy = lift . satisfy
  satisfy8 = lift . satisfy8
  someSpace = lift someSpace
  semi = lift semi
  highlightInterval h s e  = lift $ highlightInterval h s e
  nesting (Strict.StateT m) = Strict.StateT $ nesting . m
  skipping = lift . skipping
  position = lift position
  slicedWith f (Strict.StateT m) = Strict.StateT $ \s -> slicedWith (\(a,s') b -> (f a b, s')) $ m s
  lookAhead (Strict.StateT m) = Strict.StateT $ lookAhead . m

instance MonadParser m => MonadParser (ReaderT e m) where
  try (ReaderT m) = ReaderT $ try . m
  labels (ReaderT m) ss = ReaderT $ \s -> labels (m s) ss
  line = lift line
  unexpected = lift . unexpected
  satisfy = lift . satisfy
  satisfy8 = lift . satisfy8
  someSpace = lift someSpace
  semi = lift semi
  highlightInterval h s e  = lift $ highlightInterval h s e
  nesting (ReaderT m) = ReaderT $ nesting . m
  skipping = lift . skipping
  position = lift position
  slicedWith f (ReaderT m) = ReaderT $ slicedWith f . m
  lookAhead (ReaderT m) = ReaderT $ lookAhead . m

instance (MonadParser m, Monoid w) => MonadParser (Strict.WriterT w m) where
  try (Strict.WriterT m) = Strict.WriterT $ try m
  labels (Strict.WriterT m) ss = Strict.WriterT $ labels m ss
  line = lift line
  unexpected = lift . unexpected
  satisfy = lift . satisfy
  satisfy8 = lift . satisfy8
  someSpace = lift someSpace
  semi = lift semi
  highlightInterval h s e  = lift $ highlightInterval h s e
  nesting (Strict.WriterT m) = Strict.WriterT $ nesting m
  skipping = lift . skipping
  position = lift position
  slicedWith f (Strict.WriterT m) = Strict.WriterT $ slicedWith (\(a,s') b -> (f a b, s')) m
  lookAhead (Strict.WriterT m) = Strict.WriterT $ lookAhead m

instance (MonadParser m, Monoid w) => MonadParser (Lazy.WriterT w m) where
  try (Lazy.WriterT m) = Lazy.WriterT $ try m
  labels (Lazy.WriterT m) ss = Lazy.WriterT $ labels m ss
  line = lift line
  unexpected = lift . unexpected
  satisfy = lift . satisfy
  satisfy8 = lift . satisfy8
  someSpace = lift someSpace
  semi = lift semi
  highlightInterval h s e  = lift $ highlightInterval h s e
  nesting (Lazy.WriterT m) = Lazy.WriterT $ nesting m
  skipping = lift . skipping
  position = lift position
  slicedWith f (Lazy.WriterT m) = Lazy.WriterT $ slicedWith (\(a,s') b -> (f a b, s')) m
  lookAhead (Lazy.WriterT m) = Lazy.WriterT $ lookAhead m

instance (MonadParser m, Monoid w) => MonadParser (Lazy.RWST r w s m) where
  try (Lazy.RWST m) = Lazy.RWST $ \r s -> try (m r s)
  labels (Lazy.RWST m) ss = Lazy.RWST $ \r s -> labels (m r s) ss
  line = lift line
  unexpected = lift . unexpected
  satisfy = lift . satisfy
  satisfy8 = lift . satisfy8
  someSpace = lift someSpace
  semi = lift semi
  highlightInterval h s e  = lift $ highlightInterval h s e
  nesting (Lazy.RWST m) = Lazy.RWST $ \r s -> nesting (m r s)
  skipping = lift . skipping
  position = lift position
  slicedWith f (Lazy.RWST m) = Lazy.RWST $ \r s -> slicedWith (\(a,s',w) b -> (f a b, s',w)) $ m r s
  lookAhead (Lazy.RWST m) = Lazy.RWST $ \r s -> lookAhead $ m r s

instance (MonadParser m, Monoid w) => MonadParser (Strict.RWST r w s m) where
  try (Strict.RWST m) = Strict.RWST $ \r s -> try (m r s)
  labels (Strict.RWST m) ss = Strict.RWST $ \r s -> labels (m r s) ss
  line = lift line
  unexpected = lift . unexpected
  satisfy = lift . satisfy
  satisfy8 = lift . satisfy8
  someSpace = lift someSpace
  semi = lift semi
  highlightInterval h s e  = lift $ highlightInterval h s e
  nesting (Strict.RWST m) = Strict.RWST $ \r s -> nesting (m r s)
  skipping = lift . skipping
  position = lift position
  slicedWith f (Strict.RWST m) = Strict.RWST $ \r s -> slicedWith (\(a,s',w) b -> (f a b, s',w)) $ m r s
  lookAhead (Strict.RWST m) = Strict.RWST $ \r s -> lookAhead $ m r s

instance MonadParser m => MonadParser (IdentityT m) where
  try = IdentityT . try . runIdentityT
  labels (IdentityT m) ss = IdentityT $ labels m ss
  line = lift line
  unexpected = lift . unexpected
  satisfy = lift . satisfy
  satisfy8 = lift . satisfy8
  someSpace = lift someSpace
  semi = lift semi
  highlightInterval h s e  = lift $ highlightInterval h s e
  nesting (IdentityT m) = IdentityT $ nesting m
  skipping = lift . skipping
  position = lift position
  slicedWith f (IdentityT m) = IdentityT $ slicedWith f m
  lookAhead (IdentityT m) = IdentityT $ lookAhead m

instance MonadParser m => MonadParser (Yoneda m) where
  try = lift . try . lowerYoneda
  labels m ss = lift $ labels (lowerYoneda m) ss
  line = lift line
  unexpected = lift . unexpected
  satisfy = lift . satisfy
  satisfy8 = lift . satisfy8
  someSpace = lift someSpace
  semi = lift semi
  highlightInterval h s e  = lift $ highlightInterval h s e
  nesting (Yoneda m) = Yoneda $ \f -> nesting (m f)
  skipping = lift . skipping
  position = lift position
  slicedWith f = lift . slicedWith f . lowerYoneda
  lookAhead = lift . lookAhead . lowerYoneda

-- | Skip zero or more bytes worth of white space. More complex parsers are 
-- free to consider comments as white space.
whiteSpace :: MonadParser m => m ()
whiteSpace = someSpace <|> return ()
{-# INLINE whiteSpace #-}

satisfyAscii :: MonadParser m => (Char -> Bool) -> m Char
satisfyAscii p = w2c <$> satisfy8 (\w -> w <= 0x7f && p (w2c w))
{-# INLINE satisfyAscii #-}

-- | grab the remainder of the current line
restOfLine :: MonadParser m => m ByteString
restOfLine = do
  m <- position
  Strict.drop (fromIntegral (columnByte m)) <$> line
{-# INLINE restOfLine #-}

-- | label a parser with a name
(<?>) :: MonadParser m => m a -> String -> m a
p <?> msg = labels p [msg]
{-# INLINE (<?>) #-}

-- | run a parser, grabbing all of the text between its start and end points and discarding the original result
sliced :: MonadParser m => m a -> m ByteString
sliced = slicedWith (\_ bs -> bs)
{-# INLINE sliced #-}

rend :: MonadParser m => m Rendering
rend = rendering <$> position <*> line
{-# INLINE rend #-}

-- | run a parser, highlighting all of the text between its start and end points.
highlight :: MonadParser m => Highlight -> m a -> m a
highlight h p = do
  m <- position
  x <- p
  r <- position
  x <$ highlightInterval h m r
{-# INLINE highlight #-}

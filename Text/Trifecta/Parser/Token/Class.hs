-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Trifecta.Parser.Token.Class
-- Copyright   :  (c) Edward Kmett 2011
-- License     :  BSD3
-- 
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable
-- 
-----------------------------------------------------------------------------
module Text.Trifecta.Parser.Token.Class
  ( MonadTokenParser(..)
  ) where

import Control.Applicative
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Lazy as Lazy
import Control.Monad.Trans.State.Strict as Strict
import Control.Monad.Trans.Writer.Lazy as Lazy
import Control.Monad.Trans.Writer.Strict as Strict
import Control.Monad.Trans.RWS.Lazy as Lazy
import Control.Monad.Trans.RWS.Strict as Strict
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Identity
import Data.Monoid
import Text.Trifecta.Parser.Class

class MonadParser m => MonadTokenParser m where
  -- | Parses any white space. White space consists of /zero/ or more
  -- occurrences of a 'space', a line comment or a block (multi
  -- line) comment. Block comments may be nested. How comments are
  -- started and ended is defined by this method.
  whiteSpace :: m ()

  -- | Called when we enter a nested pair of symbols. Used to disable
  -- layout or highlight nested contexts.
  nesting :: m a -> m a

  -- | Lexeme parser |semi| parses the character \';\' and skips any
  -- trailing white space. Returns the character \';\'.‗
  semi :: m Char

instance MonadTokenParser m => MonadTokenParser (ReaderT r m) where
  whiteSpace = lift whiteSpace
  nesting (ReaderT m) = ReaderT $ \r -> nesting (m r)
  semi = lift semi

instance MonadTokenParser m => MonadTokenParser (Lazy.StateT s m) where
  whiteSpace = lift whiteSpace
  nesting (Lazy.StateT m) = Lazy.StateT $ \s -> nesting (m s)
  semi = lift semi

instance MonadTokenParser m => MonadTokenParser (Strict.StateT s m) where
  whiteSpace = lift whiteSpace
  nesting (Strict.StateT m) = Strict.StateT $ \s -> nesting (m s)
  semi = lift semi

instance (MonadTokenParser m, Monoid w) => MonadTokenParser (Lazy.WriterT w m) where
  whiteSpace = lift whiteSpace
  nesting (Lazy.WriterT m) = Lazy.WriterT $ nesting m
  semi = lift semi

instance (MonadTokenParser m, Monoid w) => MonadTokenParser (Strict.WriterT w m) where
  whiteSpace = lift whiteSpace
  nesting (Strict.WriterT m) = Strict.WriterT $ nesting m
  semi = lift semi

instance (MonadTokenParser m, Monoid w) => MonadTokenParser (Lazy.RWST r w s m) where
  whiteSpace = lift whiteSpace
  nesting (Lazy.RWST m) = Lazy.RWST $ \r s -> nesting (m r s)
  semi = lift semi

instance (MonadTokenParser m, Monoid w) => MonadTokenParser (Strict.RWST r w s m) where
  whiteSpace = lift whiteSpace
  nesting (Strict.RWST m) = Strict.RWST $ \r s -> nesting (m r s)
  semi = lift semi

instance MonadTokenParser m => MonadTokenParser (IdentityT m) where
  whiteSpace = lift whiteSpace
  nesting (IdentityT m) = IdentityT (nesting m)
  semi = lift semi

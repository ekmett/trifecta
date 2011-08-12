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
  whiteSpace       :: m ()
  
  -- | @lexeme p@ first applies parser @p@ and than the 'whiteSpace'
  -- parser, returning the value of @p@. Every lexical
  -- token (lexeme) is defined using @lexeme@, this way every parse
  -- starts at a point without white space. Parsers that use @lexeme@ are
  -- called /lexeme/ parsers in this document.
  -- 
  -- The only point where the 'whiteSpace' parser should be
  -- called explicitly is the start of the main parser in order to skip
  -- any leading white space.
  --
  -- >    mainParser  = do { whiteSpace
  -- >                     ; ds <- many (lexeme digit)
  -- >                     ; eof
  -- >                     ; return (sum ds)
  -- >                     }
  lexeme :: m a -> m a
  lexeme p = p <* whiteSpace

instance MonadTokenParser m => MonadTokenParser (ReaderT r m) where
  whiteSpace = lift whiteSpace
  lexeme (ReaderT m) = ReaderT $ lexeme . m

instance MonadTokenParser m => MonadTokenParser (Lazy.StateT s m) where
  whiteSpace = lift whiteSpace
  lexeme (Lazy.StateT m) = Lazy.StateT $ lexeme . m

instance MonadTokenParser m => MonadTokenParser (Strict.StateT s m) where
  whiteSpace = lift whiteSpace
  lexeme (Strict.StateT m) = Strict.StateT $ lexeme . m

instance (MonadTokenParser m, Monoid w) => MonadTokenParser (Lazy.WriterT w m) where
  whiteSpace = lift whiteSpace
  lexeme (Lazy.WriterT m) = Lazy.WriterT $ lexeme m

instance (MonadTokenParser m, Monoid w) => MonadTokenParser (Strict.WriterT w m) where
  whiteSpace = lift whiteSpace
  lexeme (Strict.WriterT m) = Strict.WriterT $ lexeme m

instance (MonadTokenParser m, Monoid w) => MonadTokenParser (Lazy.RWST r w s m) where
  whiteSpace = lift whiteSpace
  lexeme (Lazy.RWST m) = Lazy.RWST $ \r s -> lexeme (m r s)

instance (MonadTokenParser m, Monoid w) => MonadTokenParser (Strict.RWST r w s m) where
  whiteSpace = lift whiteSpace
  lexeme (Strict.RWST m) = Strict.RWST $ \r s -> lexeme (m r s)

instance MonadTokenParser m => MonadTokenParser (IdentityT m) where
  whiteSpace = lift whiteSpace
  lexeme = IdentityT . lexeme . runIdentityT

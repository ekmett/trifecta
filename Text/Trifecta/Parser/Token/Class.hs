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

import Control.Monad.Trans.Class
import Data.ByteString
import Control.Monad.Trans.State.Lazy as Lazy
import Text.Trifecta.Parser.Class

class MonadParser m => MonadTokenParser m where
  -- | Parses any white space. White space consists of /zero/ or more
  -- occurrences of a 'space', a line comment or a block (multi
  -- line) comment. Block comments may be nested. How comments are
  -- started and ended is defined by this method.
  whiteSpace       :: m ()

  -- | This parser should accept any start characters of identifiers. For
  -- example @letter \<|> char \"_\"@. 
  identStart       :: m Char

  -- | This parser should accept any legal tail characters of identifiers.
  -- For example @alphaNum \<|> char \"_\"@. 
  identLetter      :: m Char

  -- | This parser should accept any start characters of operators. For
  -- example @oneOf \":!#$%&*+.\/\<=>?\@\\\\^|-~\"@  
  opStart          :: m Char

  -- | This parser should accept any legal tail characters of operators.
  -- Note that this parser should even be defined if the language doesn't
  -- support user-defined operators, or otherwise the 'reservedOp'
  -- parser won't work correctly.  
  opLetter         :: m Char

  -- | Check the list of reserved identifiers.  
  isReservedName   :: ByteString -> m Bool

  -- | Check the list of reserved operators. 
  isReservedOpName :: ByteString -> m Bool

instance MonadTokenParser m => MonadTokenParser (Lazy.StateT s m) where
  whiteSpace = lift whiteSpace
  identStart = lift identStart
  identLetter = lift identLetter
  opStart = lift opStart
  opLetter = lift opLetter
  isReservedName = lift . isReservedName
  isReservedOpName = lift . isReservedOpName


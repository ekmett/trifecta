-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Trifecta.Parser.Token.Identifier
-- Copyright   :  (c) Edward Kmett 2011
-- License     :  BSD3
-- 
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  provisional
-- Portability :  non-portable
-- 
-----------------------------------------------------------------------------
module Text.Trifecta.Parser.Token.Identifier
  ( IdentifierStyle(..)
  , ident
  , reserved
  , reservedByteString
  ) where

import Data.ByteString as Strict hiding (map, zip, foldl, foldr)
import Data.ByteString.UTF8 as UTF8
import Data.HashSet as HashSet
import Control.Applicative
import Control.Monad (when)
import Text.Trifecta.Parser.Class
import Text.Trifecta.Parser.Char
import Text.Trifecta.Parser.Combinators
import Text.Trifecta.Parser.Token.Class

data IdentifierStyle m = IdentifierStyle
  { styleName              :: String
  , styleStart             :: m ()
  , styleLetter            :: m ()
  , styleReserved          :: HashSet ByteString
  , styleHighlight         :: TokenHighlight
  , styleReservedHighlight :: TokenHighlight
  }

-- | parse a reserved operator or identifier
reserved :: MonadTokenParser m => IdentifierStyle m -> String -> m ()
reserved s name = reservedByteString s $! UTF8.fromString name

-- | parse a reserved operator or identifier specified by bytestring
reservedByteString :: MonadTokenParser m => IdentifierStyle m -> ByteString -> m ()
reservedByteString s name = lexeme $ try $ do
   _ <- highlightToken (styleReservedHighlight s) $ byteString name 
   notFollowedBy (styleLetter s) <?> "end of " ++ show name

-- | parse an non-reserved identifier or symbol
ident :: MonadTokenParser m => IdentifierStyle m -> m ByteString
ident s = lexeme $ try $ do
  name <- highlightToken (styleHighlight s) (sliced (styleStart s *> skipMany (styleLetter s))) <?> styleName s
  when (member name (styleReserved s)) $ unexpected $ "reserved " ++ styleName s ++ " " ++ show name
  return name


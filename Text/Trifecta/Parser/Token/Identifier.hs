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
-- idStyle = haskellIdentifierStyle { styleReserved = ... } 
-- identifier = ident haskellIdentifierStyle
-- reserved   = reserve haskellIdentifierStyle
--
-----------------------------------------------------------------------------
module Text.Trifecta.Parser.Token.Identifier
  ( IdentifierStyle(..)
  , ident
  , reserve
  , reserveByteString
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
import Text.Trifecta.Highlight.Prim

data IdentifierStyle m = IdentifierStyle
  { styleName              :: String
  , styleStart             :: m ()
  , styleLetter            :: m ()
  , styleReserved          :: HashSet ByteString
  , styleHighlight         :: Highlight
  , styleReservedHighlight :: Highlight
  }

-- | parse a reserved operator or identifier using a given style
reserve :: MonadTokenParser m => IdentifierStyle m -> String -> m ()
reserve s name = reserveByteString s $! UTF8.fromString name

-- | parse a reserved operator or identifier using a given style specified by bytestring
reserveByteString :: MonadTokenParser m => IdentifierStyle m -> ByteString -> m ()
reserveByteString s name = lexeme $ try $ do
   _ <- highlight (styleReservedHighlight s) $ byteString name 
   notFollowedBy (styleLetter s) <?> "end of " ++ show name

-- | parse an non-reserved identifier or symbol
ident :: MonadTokenParser m => IdentifierStyle m -> m ByteString
ident s = lexeme $ try $ do
  name <- highlight (styleHighlight s) (sliced (styleStart s *> skipMany (styleLetter s))) <?> styleName s
  when (member name (styleReserved s)) $ unexpected $ "reserved " ++ styleName s ++ " " ++ show name
  return name


-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Trifecta.Parser.Identifier
-- Copyright   :  (c) Edward Kmett 2011
-- License     :  BSD3
-- 
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  provisional
-- Portability :  non-portable
--
-- > idStyle = haskellIdentifierStyle { styleReserved = ... }
-- > identifier = ident haskellIdentifierStyle
-- > reserved   = reserve haskellIdentifierStyle
--
-----------------------------------------------------------------------------
module Text.Trifecta.Parser.Identifier
  ( IdentifierStyle(..)
  , liftIdentifierStyle
  , ident
  , reserve
  , reserveByteString
  ) where

import Data.ByteString as Strict hiding (map, zip, foldl, foldr)
import Data.ByteString.UTF8 as UTF8
import Data.HashSet as HashSet
import Control.Applicative
import Control.Monad (when)
import Control.Monad.Trans.Class
import Text.Trifecta.Parser.Class
import Text.Trifecta.Parser.Char
import Text.Trifecta.Parser.Combinators
import Text.Trifecta.Parser.Token.Combinators
import Text.Trifecta.Highlight.Prim

data IdentifierStyle m = IdentifierStyle
  { styleName              :: String
  , styleStart             :: m ()
  , styleLetter            :: m ()
  , styleReserved          :: HashSet ByteString
  , styleHighlight         :: Highlight
  , styleReservedHighlight :: Highlight
  }

-- | Lift an identifier style into a monad transformer
liftIdentifierStyle :: (MonadTrans t, Monad m) => IdentifierStyle m -> IdentifierStyle (t m)
liftIdentifierStyle s =
  s { styleStart  = lift (styleStart s)
    , styleLetter = lift (styleLetter s)
    }

-- | parse a reserved operator or identifier using a given style
reserve :: MonadParser m => IdentifierStyle m -> String -> m ()
reserve s name = reserveByteString s $! UTF8.fromString name

-- | parse a reserved operator or identifier using a given style specified by bytestring
reserveByteString :: MonadParser m => IdentifierStyle m -> ByteString -> m ()
reserveByteString s name = lexeme $ try $ do
   _ <- highlight (styleReservedHighlight s) $ byteString name
   notFollowedBy (styleLetter s) <?> "end of " ++ show name

-- | parse an non-reserved identifier or symbol
ident :: MonadParser m => IdentifierStyle m -> m ByteString
ident s = lexeme $ try $ do
  name <- highlight (styleHighlight s) (sliced (styleStart s *> skipMany (styleLetter s))) <?> styleName s
  when (member name (styleReserved s)) $ unexpected $ "reserved " ++ styleName s ++ " " ++ show name
  return name

-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Trifecta.Parser.Token.Style
-- Copyright   :  (c) Edward Kmett 2011
-- License     :  BSD3
-- 
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  provisional
-- Portability :  non-portable
-- 
-----------------------------------------------------------------------------
module Text.Trifecta.Parser.Token.Style
  ( Style(..)
 
  -- identifier styles
  , emptyIdents, haskellIdents, haskell98Idents

  -- operator styles
  , emptyOps, haskellOps, haskell98Ops

  -- reading identifiers
  , ident
  , reserved
  , reservedByteString
  ) where

import Data.ByteString as Strict hiding (map, zip, foldl, foldr)
import Data.ByteString.UTF8 as UTF8
import Data.HashSet as HashSet
import Data.Monoid
import Control.Applicative
import Control.Monad (when)
import Text.Trifecta.Parser.Class
import Text.Trifecta.Parser.Char
import Text.Trifecta.Parser.Combinators
import Text.Trifecta.Parser.Token.Class

data Style m = Style
  { styleName         :: String
  , styleStart        :: m ()
  , styleLetter       :: m ()
  , styleReserved     :: HashSet ByteString
  }

-- | parse a reserved operator or identifier
reserved :: MonadTokenParser m => Style m -> String -> m ()
reserved s name = reservedByteString s $! UTF8.fromString name

-- | parse a reserved operator or identifier specified by bytestring
reservedByteString :: MonadTokenParser m => Style m -> ByteString -> m ()
reservedByteString s name = lexeme $ try $ do
   _ <- byteString name 
   notFollowedBy (styleLetter s) <?> "end of " ++ show name

-- | parse an non-reserved identifier or symbol
ident :: MonadTokenParser m => Style m -> m ByteString
ident s = lexeme $ try $ do
  name <- sliced (styleStart s *> skipMany (styleLetter s)) <?> styleName s
  when (member name (styleReserved s)) $ unexpected $ "reserved " ++ styleName s ++ " " ++ show name
  return name

set :: [String] -> HashSet ByteString
set = HashSet.fromList . fmap UTF8.fromString

emptyOps, haskell98Ops, haskellOps :: MonadTokenParser m => Style m
emptyOps = Style
  { styleName     = "operator"
  , styleStart    = styleLetter emptyOps
  , styleLetter   = () <$ oneOf ":!#$%&*+./<=>?@\\^|-~"
  , styleReserved = mempty
  }
haskell98Ops = emptyOps 
  { styleReserved = set ["::","..","=","\\","|","<-","->","@","~","=>"]
  }
haskellOps = haskell98Ops

emptyIdents, haskell98Idents, haskellIdents :: MonadTokenParser m => Style m
emptyIdents = Style
  { styleName     = "identifier"
  , styleStart    = () <$ (letter <|> char '_')
  , styleLetter   = () <$ (alphaNum <|> oneOf "_'")
  , styleReserved = set [] }

haskell98Idents = emptyIdents
  { styleReserved = set haskell98ReservedIdents }
haskellIdents = haskell98Idents
  { styleLetter	  = styleLetter haskell98Idents <|> () <$ char '#'
  , styleReserved = set $ haskell98ReservedIdents ++
      ["foreign","import","export","primitive","_ccall_","_casm_" ,"forall"]
  }

haskell98ReservedIdents :: [String]
haskell98ReservedIdents = 
  ["let","in","case","of","if","then","else","data","type"
  ,"class","default","deriving","do","import","infix"
  ,"infixl","infixr","instance","module","newtype"
  ,"where","primitive" -- "as","qualified","hiding"
  ]

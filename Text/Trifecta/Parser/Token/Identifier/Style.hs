-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Trifecta.Parser.Token.Identifier.Style
-- Copyright   :  (c) Edward Kmett 2011
-- License     :  BSD3
-- 
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  provisional
-- Portability :  non-portable
-- 
-----------------------------------------------------------------------------
module Text.Trifecta.Parser.Token.Identifier.Style
  ( 
  -- identifier styles
    emptyIdents, haskellIdents, haskell98Idents
  -- operator styles
  , emptyOps, haskellOps, haskell98Ops
  ) where

import Data.ByteString as Strict hiding (map, zip, foldl, foldr)
import Data.ByteString.UTF8 as UTF8
import Data.HashSet as HashSet
import Data.Monoid
import Control.Applicative
import Text.Trifecta.Parser.Char
import Text.Trifecta.Parser.Token.Class
import Text.Trifecta.Parser.Token.Identifier

set :: [String] -> HashSet ByteString
set = HashSet.fromList . fmap UTF8.fromString

emptyOps, haskell98Ops, haskellOps :: MonadTokenParser m => IdentifierStyle m
emptyOps = IdentifierStyle
  { styleName     = "operator"
  , styleStart    = styleLetter emptyOps
  , styleLetter   = () <$ oneOf ":!#$%&*+./<=>?@\\^|-~"
  , styleReserved = mempty
  }
haskell98Ops = emptyOps 
  { styleReserved = set ["::","..","=","\\","|","<-","->","@","~","=>"]
  }
haskellOps = haskell98Ops

emptyIdents, haskell98Idents, haskellIdents :: MonadTokenParser m => IdentifierStyle m
emptyIdents = IdentifierStyle
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

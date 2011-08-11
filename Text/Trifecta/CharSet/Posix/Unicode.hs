-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Trifecta.CharSet.Posix.Unicode
-- Copyright   :  (c) Edward Kmett 2010
-- License     :  BSD3
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-------------------------------------------------------------------------------

module Text.Trifecta.CharSet.Posix.Unicode
    ( posixUnicode
    , lookupPosixUnicodeCharSet
    -- * POSIX ASCII \"classes\"
    , alnum, alpha, ascii, blank, cntrl, digit, graph, print, word, punct, space, upper, lower, xdigit
    ) where

import Prelude hiding (print)
import Data.Char
import Text.Trifecta.CharSet.Prim
import qualified Text.Trifecta.CharSet.Unicode.Category as Category
import qualified Text.Trifecta.CharSet.Unicode.Block as Block
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap

alnum, alpha, ascii, blank, cntrl, digit, graph, print, word, punct, space, upper, lower, xdigit :: CharSet
alnum = alpha `union` digit
ascii = Block.basicLatin
alpha = Category.letterAnd
blank = insert '\t' Category.space 
cntrl = Category.control
digit = Category.decimalNumber
lower = Category.lowercaseLetter
upper = Category.uppercaseLetter
graph = complement (Category.separator `union` Category.other)
print = complement (Category.other)
word  = Category.letter `union` Category.number `union` Category.connectorPunctuation
punct = Category.punctuation `union` Category.symbol
space = fromList " \t\r\n\v\f" `union` Category.separator
xdigit = digit `union` range 'a' 'f' `union` range 'A' 'F'

-- :digit:, etc.
posixUnicode :: HashMap String CharSet
posixUnicode = HashMap.fromList
    [ ("alnum", alnum)
    , ("alpha", alpha)
    , ("ascii", ascii)
    , ("blank", blank)
    , ("cntrl", cntrl)
    , ("digit", digit)
    , ("graph", graph) 
    , ("print", print)
    , ("word",  word)
    , ("punct", punct)
    , ("space", space)
    , ("upper", upper)
    , ("lower", lower)
    , ("xdigit", xdigit)
    ]

lookupPosixUnicodeCharSet :: String -> Maybe CharSet
lookupPosixUnicodeCharSet s = HashMap.lookup (Prelude.map toLower s) posixUnicode


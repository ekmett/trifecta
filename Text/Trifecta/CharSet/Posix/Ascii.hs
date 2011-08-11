-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Trifecta.CharSet.Posix.Ascii
-- Copyright   :  (c) Edward Kmett 2010
-- License     :  BSD3
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-------------------------------------------------------------------------------

module Text.Trifecta.CharSet.Posix.Ascii
    ( posixAscii
    , lookupPosixAsciiCharSet
    -- * Traditional POSIX ASCII \"classes\"
    , alnum, alpha, ascii, blank, cntrl, digit, graph, print, word, punct, space, upper, lower, xdigit
    ) where

import Prelude hiding (print)
import Data.Char
import Text.Trifecta.CharSet
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap

alnum, alpha, ascii, blank, cntrl, digit, graph, print, word, punct, space, upper, lower, xdigit :: CharSet
alnum = alpha `union` digit
alpha = lower `union` upper
ascii = range '\x00' '\x7f'
blank = fromList " \t"
cntrl = insert '\x7f' $ range '\x00' '\x1f'
digit = range '0' '9'
lower = range 'a' 'z'
upper = range 'A' 'Z'
graph = range '\x21' '\x7e'
print = insert '\x20' graph
word  = insert '_' alnum
punct = fromList "-!\"#$%&'()*+,./:;<=>?@[\\]^_`{|}~"
space = fromList " \t\r\n\v\f"
xdigit = digit `union` range 'a' 'f' `union` range 'A' 'F'

-- :digit:, etc.
posixAscii :: HashMap String CharSet
posixAscii = HashMap.fromList
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

lookupPosixAsciiCharSet :: String -> Maybe CharSet
lookupPosixAsciiCharSet s = HashMap.lookup (Prelude.map toLower s) posixAscii

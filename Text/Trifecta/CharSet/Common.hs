-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Trifecta.CharSet.Common
-- Copyright   :  (c) Edward Kmett 2010-2011
-- License     :  BSD3
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- The various character classifications from "Data.Char" as 'CharSet's
-------------------------------------------------------------------------------

module Text.Trifecta.CharSet.Common
    ( 
    -- ** Data.Char classes
      control
    , space
    , lower
    , upper
    , alpha
    , alphaNum
    , print
    , digit
    , octDigit
    , letter
    , mark
    , number
    , punctuation
    , symbol
    , separator
    , ascii
    , latin1
    , asciiUpper
    , asciiLower
    ) where

import Prelude ()
import Data.Char
import Text.Trifecta.CharSet.Prim

-- Haskell character classes from Data.Char
control, space, lower, upper, alpha, alphaNum, 
  print, digit, octDigit, letter, mark, number, 
  punctuation, symbol, separator, ascii, latin1
  , asciiUpper, asciiLower :: CharSet

control = build isControl
space = build isSpace
lower = build isLower
upper = build isUpper
alpha = build isAlpha
alphaNum = build isAlphaNum
print = build isPrint
digit = build isDigit
octDigit = build isOctDigit
letter = build isLetter
mark = build isMark
number = build isNumber
punctuation = build isPunctuation
symbol = build isSymbol
separator = build isSeparator
ascii = build isAscii
latin1 = build isLatin1
asciiUpper = build isAsciiUpper
asciiLower = build isAsciiLower

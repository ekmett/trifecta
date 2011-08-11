{-# LANGUAGE DeriveDataTypeable #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Trifecta.CharSet.Unicode
-- Copyright   :  (c) Edward Kmett 2010
-- License     :  BSD3
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Provides unicode general categories, which are typically connoted by 
-- @\p{Ll}@ or @\p{Modifier_Letter}@. Lookups can be constructed using 'categories'
-- or individual character sets can be used directly.
-------------------------------------------------------------------------------

module Text.Trifecta.CharSet.Unicode
    ( 
    -- * Unicode General Category
      UnicodeCategory(..)
    -- * Lookup
    , unicodeCategories
    -- * CharSets by UnicodeCategory
    -- ** Letter
    , modifierLetter, otherLetter, letter
    -- *** Letter\&
    , lowercaseLetter, uppercaseLetter, titlecaseLetter, letterAnd
    -- ** Mark
    , nonSpacingMark, spacingCombiningMark, enclosingMark, mark
    -- ** Separator
    , space, lineSeparator, paragraphSeparator, separator
    -- ** Symbol
    , mathSymbol, currencySymbol, modifierSymbol, otherSymbol, symbol
    -- ** Number
    , decimalNumber, letterNumber, otherNumber, number
    -- ** Punctuation
    , dashPunctuation, openPunctuation, closePunctuation, initialQuote
    , finalQuote, connectorPunctuation, otherPunctuation, punctuation 
    -- ** Other
    , control, format, privateUse, surrogate, notAssigned, other
    ) where

import Data.Char
import Data.Data
import Text.Trifecta.CharSet.Prim

data UnicodeCategory = UnicodeCategory String String CharSet String
    deriving (Show, Data, Typeable)

-- \p{Letter} or \p{Mc}
unicodeCategories :: [UnicodeCategory]
unicodeCategories =
    [ UnicodeCategory "Letter" "L" letter "any kind of letter from any language."
    ,     UnicodeCategory "Lowercase_Letter" "Ll" lowercaseLetter "a lowercase letter that has an uppercase variant"
    ,     UnicodeCategory "Uppercase_Letter" "Lu" uppercaseLetter "an uppercase letter that has a lowercase variant"
    ,     UnicodeCategory "Titlecase_Letter" "Lt" titlecaseLetter "a letter that appears at the start of a word when only the first letter of the word is capitalized"
    ,     UnicodeCategory "Letter&" "L&" letterAnd "a letter that exists in lowercase and uppercase variants (combination of Ll, Lu and Lt)"
    ,     UnicodeCategory "Modifier_Letter" "Lm" modifierLetter "a special character that is used like a letter"
    ,     UnicodeCategory "Other_Letter" "Lo" otherLetter "a letter or ideograph that does not have lowercase and uppercase variants"
    , UnicodeCategory "Mark" "M" mark "a character intended to be combined with another character (e.g. accents, umlauts, enclosing boxes, etc.)"
    ,     UnicodeCategory "Non_Spacing_Mark" "Mn" nonSpacingMark "a character intended to be combined with another character without taking up extra space (e.g. accents, umlauts, etc.)"
    ,     UnicodeCategory "Spacing_Combining_Mark" "Mc" spacingCombiningMark "a character intended to be combined with another character that takes up extra space (vowel signs in many Eastern languages)"
    ,     UnicodeCategory "Enclosing_Mark" "Me" enclosingMark "a character that encloses the character is is combined with (circle, square, keycap, etc.)"
    , UnicodeCategory "Separator" "Z" separator "any kind of whitespace or invisible separator"
    ,     UnicodeCategory "Space_Separator" "Zs" space "a whitespace character that is invisible, but does take up space"
    ,     UnicodeCategory "Line_Separator" "Zl" lineSeparator "line separator character U+2028"
    ,     UnicodeCategory "Paragraph_Separator" "Zp" paragraphSeparator "paragraph separator character U+2029"
    , UnicodeCategory "Symbol" "S" symbol "math symbols, currency signs, dingbats, box-drawing characters, etc."
    ,     UnicodeCategory "Math_Symbol" "Sm" mathSymbol "any mathematical symbol"
    ,     UnicodeCategory "Currency_Symbol" "Sc" currencySymbol "any currency sign"
    ,     UnicodeCategory "Modifier_Symbol" "Sk" modifierSymbol "a combining character (mark) as a full character on its own"
    ,     UnicodeCategory "Other_Symbol" "So" otherSymbol "various symbols that are not math symbols, currency signs, or combining characters"
    , UnicodeCategory "Number" "N" number "any kind of numeric character in any script"
    ,     UnicodeCategory "Decimal_Digit_Number" "Nd" decimalNumber "a digit zero through nine in any script except ideographic scripts"
    ,     UnicodeCategory "Letter_Number" "Nl" letterNumber "a number that looks like a letter, such as a Roman numeral"
    ,     UnicodeCategory "Other_Number" "No" otherNumber "a superscript or subscript digit, or a number that is not a digit 0..9 (excluding numbers from ideographic scripts)"
    , UnicodeCategory "Punctuation" "P" punctuation "any kind of punctuation character"
    ,     UnicodeCategory "Dash_Punctuation" "Pd" dashPunctuation "any kind of hyphen or dash"
    ,     UnicodeCategory "Open_Punctuation" "Ps" openPunctuation "any kind of opening bracket"
    ,     UnicodeCategory "Close_Punctuation" "Pe" closePunctuation "any kind of closing bracket"
    ,     UnicodeCategory "Initial_Punctuation" "Pi" initialQuote "any kind of opening quote"
    ,     UnicodeCategory "Final_Punctuation" "Pf" finalQuote "any kind of closing quote"
    ,     UnicodeCategory "Connector_Punctuation" "Pc" connectorPunctuation "a punctuation character such as an underscore that connects words"
    ,     UnicodeCategory "Other_Punctuation" "Po" otherPunctuation "any kind of punctuation character that is not a dash, bracket, quote or connector"
    , UnicodeCategory "Other" "C" other "invisible control characters and unused code points"
    ,     UnicodeCategory "Control" "Cc" control "an ASCII 0x00..0x1F or Latin-1 0x80..0x9F control character"
    ,     UnicodeCategory "Format" "Cf" format "invisible formatting indicator"
    ,     UnicodeCategory "Private_Use" "Co" privateUse "any code point reserved for private use"
    ,     UnicodeCategory "Surrogate" "Cs" surrogate "one half of a surrogate pair in UTF-16 encoding"
    ,     UnicodeCategory "Unassigned" "Cn" notAssigned "any code point to which no character has been assigned.properties" ]

cat :: GeneralCategory -> CharSet
cat category = build ((category ==) . generalCategory)

-- Letter
lowercaseLetter, uppercaseLetter, titlecaseLetter, letterAnd, modifierLetter, otherLetter, letter :: CharSet
lowercaseLetter = cat LowercaseLetter
uppercaseLetter = cat UppercaseLetter
titlecaseLetter = cat TitlecaseLetter
letterAnd = lowercaseLetter 
    `union` uppercaseLetter 
    `union` titlecaseLetter
modifierLetter  = cat ModifierLetter
otherLetter = cat OtherLetter
letter 
          = letterAnd 
    `union` modifierLetter 
    `union` otherLetter

-- Marks
nonSpacingMark, spacingCombiningMark, enclosingMark, mark :: CharSet
nonSpacingMark = cat NonSpacingMark
spacingCombiningMark = cat SpacingCombiningMark
enclosingMark = cat EnclosingMark
mark 
          = nonSpacingMark 
    `union` spacingCombiningMark 
    `union` enclosingMark

space, lineSeparator, paragraphSeparator, separator :: CharSet
space = cat Space
lineSeparator = cat LineSeparator
paragraphSeparator = cat ParagraphSeparator
separator 
          = space 
    `union` lineSeparator 
    `union` paragraphSeparator

mathSymbol, currencySymbol, modifierSymbol, otherSymbol, symbol :: CharSet
mathSymbol = cat MathSymbol
currencySymbol = cat CurrencySymbol
modifierSymbol = cat ModifierSymbol
otherSymbol = cat OtherSymbol
symbol 
          = mathSymbol 
    `union` currencySymbol 
    `union` modifierSymbol 
    `union` otherSymbol

decimalNumber, letterNumber, otherNumber, number :: CharSet
decimalNumber = cat DecimalNumber
letterNumber = cat LetterNumber
otherNumber = cat OtherNumber
number 
          = decimalNumber 
    `union` letterNumber 
    `union` otherNumber

dashPunctuation, openPunctuation, closePunctuation, initialQuote, 
  finalQuote, connectorPunctuation, otherPunctuation, punctuation :: CharSet

dashPunctuation = cat DashPunctuation
openPunctuation = cat OpenPunctuation
closePunctuation = cat ClosePunctuation
initialQuote = cat InitialQuote
finalQuote = cat FinalQuote
connectorPunctuation  = cat ConnectorPunctuation
otherPunctuation = cat OtherPunctuation
punctuation 
          = dashPunctuation 
    `union` openPunctuation 
    `union` closePunctuation 
    `union` initialQuote 
    `union` finalQuote 
    `union` connectorPunctuation 
    `union` otherPunctuation

control, format, privateUse, surrogate, notAssigned, other :: CharSet
control = cat Control
format = cat Format
privateUse = cat PrivateUse
surrogate = cat Surrogate
notAssigned = cat NotAssigned
other = control 
    `union` format 
    `union` privateUse 
    `union` surrogate 
    `union` notAssigned

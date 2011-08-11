{-# LANGUAGE DeriveDataTypeable #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Trifecta.CharSet.Unicode.Category
-- Copyright   :  (c) Edward Kmett 2010-2011
-- License     :  BSD3
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Provides unicode general categories, which are typically connoted by 
-- @\p{Ll}@ or @\p{Modifier_Letter}@. Lookups can be constructed using 'categories'
-- or individual character sets can be used directly.
-- 
-- A case, @_@ and @-@ insensitive lookup is provided by 'lookupCategory'
-- and can be used to provide behavior similar to that of Perl or PCRE.
-------------------------------------------------------------------------------

module Text.Trifecta.CharSet.Unicode.Category
    ( 
    -- * Unicode General Category
      Category(..)
    -- * Lookup
    , categories
    , lookupCategory
    , lookupCategoryCharSet
    -- * CharSets by Category
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
import Text.Trifecta.CharSet.Prim
import Data.Data
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap

data Category = Category 
    { categoryName :: String
    , categoryAbbreviation :: String
    , categoryCharSet :: CharSet
    , categoryDescription :: String
    } deriving (Show, Data, Typeable)

-- \p{Letter} or \p{Mc}
categories :: [Category]
categories =
    [ Category "Letter" "L" letter "any kind of letter from any language."
    ,     Category "Lowercase_Letter" "Ll" lowercaseLetter "a lowercase letter that has an uppercase variant"
    ,     Category "Uppercase_Letter" "Lu" uppercaseLetter "an uppercase letter that has a lowercase variant"
    ,     Category "Titlecase_Letter" "Lt" titlecaseLetter "a letter that appears at the start of a word when only the first letter of the word is capitalized"
    ,     Category "Letter&" "L&" letterAnd "a letter that exists in lowercase and uppercase variants (combination of Ll, Lu and Lt)"
    ,     Category "Modifier_Letter" "Lm" modifierLetter "a special character that is used like a letter"
    ,     Category "Other_Letter" "Lo" otherLetter "a letter or ideograph that does not have lowercase and uppercase variants"
    , Category "Mark" "M" mark "a character intended to be combined with another character (e.g. accents, umlauts, enclosing boxes, etc.)"
    ,     Category "Non_Spacing_Mark" "Mn" nonSpacingMark "a character intended to be combined with another character without taking up extra space (e.g. accents, umlauts, etc.)"
    ,     Category "Spacing_Combining_Mark" "Mc" spacingCombiningMark "a character intended to be combined with another character that takes up extra space (vowel signs in many Eastern languages)"
    ,     Category "Enclosing_Mark" "Me" enclosingMark "a character that encloses the character is is combined with (circle, square, keycap, etc.)"
    , Category "Separator" "Z" separator "any kind of whitespace or invisible separator"
    ,     Category "Space_Separator" "Zs" space "a whitespace character that is invisible, but does take up space"
    ,     Category "Line_Separator" "Zl" lineSeparator "line separator character U+2028"
    ,     Category "Paragraph_Separator" "Zp" paragraphSeparator "paragraph separator character U+2029"
    , Category "Symbol" "S" symbol "math symbols, currency signs, dingbats, box-drawing characters, etc."
    ,     Category "Math_Symbol" "Sm" mathSymbol "any mathematical symbol"
    ,     Category "Currency_Symbol" "Sc" currencySymbol "any currency sign"
    ,     Category "Modifier_Symbol" "Sk" modifierSymbol "a combining character (mark) as a full character on its own"
    ,     Category "Other_Symbol" "So" otherSymbol "various symbols that are not math symbols, currency signs, or combining characters"
    , Category "Number" "N" number "any kind of numeric character in any script"
    ,     Category "Decimal_Digit_Number" "Nd" decimalNumber "a digit zero through nine in any script except ideographic scripts"
    ,     Category "Letter_Number" "Nl" letterNumber "a number that looks like a letter, such as a Roman numeral"
    ,     Category "Other_Number" "No" otherNumber "a superscript or subscript digit, or a number that is not a digit 0..9 (excluding numbers from ideographic scripts)"
    , Category "Punctuation" "P" punctuation "any kind of punctuation character"
    ,     Category "Dash_Punctuation" "Pd" dashPunctuation "any kind of hyphen or dash"
    ,     Category "Open_Punctuation" "Ps" openPunctuation "any kind of opening bracket"
    ,     Category "Close_Punctuation" "Pe" closePunctuation "any kind of closing bracket"
    ,     Category "Initial_Punctuation" "Pi" initialQuote "any kind of opening quote"
    ,     Category "Final_Punctuation" "Pf" finalQuote "any kind of closing quote"
    ,     Category "Connector_Punctuation" "Pc" connectorPunctuation "a punctuation character such as an underscore that connects words"
    ,     Category "Other_Punctuation" "Po" otherPunctuation "any kind of punctuation character that is not a dash, bracket, quote or connector"
    , Category "Other" "C" other "invisible control characters and unused code points"
    ,     Category "Control" "Cc" control "an ASCII 0x00..0x1F or Latin-1 0x80..0x9F control character"
    ,     Category "Format" "Cf" format "invisible formatting indicator"
    ,     Category "Private_Use" "Co" privateUse "any code point reserved for private use"
    ,     Category "Surrogate" "Cs" surrogate "one half of a surrogate pair in UTF-16 encoding"
    ,     Category "Unassigned" "Cn" notAssigned "any code point to which no character has been assigned.properties" ]

lookupTable :: HashMap String Category
lookupTable = HashMap.fromList 
  [ (canonicalize x, category) 
  | category@(Category l s _ _) <- categories
  , x <- [l,s] 
  ]

lookupCategory :: String -> Maybe Category
lookupCategory s = HashMap.lookup (canonicalize s) lookupTable

lookupCategoryCharSet :: String -> Maybe CharSet
lookupCategoryCharSet = fmap categoryCharSet . lookupCategory

canonicalize :: String -> String
canonicalize s = case Prelude.map toLower s of
  'i' : 's' : xs -> go xs
  xs -> go xs
  where
    go ('-':xs) = go xs
    go ('_':xs) = go xs
    go (' ':xs) = go xs
    go (x:xs) = x : go xs
    go [] = []

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

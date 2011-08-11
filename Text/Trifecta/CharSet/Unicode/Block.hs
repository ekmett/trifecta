{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Trifecta.CharSet.Unicode.Block
-- Copyright   :  (c) Edward Kmett 2010-2011
-- License     :  BSD3
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Provides unicode general categories, which are typically connoted by 
-- @\p{InBasicLatin}@ or @\p{InIPA_Extensions}@. Lookups can be constructed using 'categories'
-- or individual character sets can be used directly.
-------------------------------------------------------------------------------

module Text.Trifecta.CharSet.Unicode.Block
    ( 
    -- * Unicode General Category
      Block(..)
    -- * Lookup
    , blocks
    , lookupBlock
    , lookupBlockCharSet
    -- * CharSets by Block
    , basicLatin
    , latin1Supplement
    , latinExtendedA
    , latinExtendedB
    , ipaExtensions
    , spacingModifierLetters
    , combiningDiacriticalMarks
    , greekAndCoptic
    , cyrillic
    , cyrillicSupplementary
    , armenian
    , hebrew
    , arabic
    , syriac
    , thaana
    , devanagari
    , bengali
    , gurmukhi
    , gujarati
    , oriya
    , tamil
    , telugu
    , kannada
    , malayalam
    , sinhala
    , thai
    , lao
    , tibetan
    , myanmar
    , georgian
    , hangulJamo
    , ethiopic
    , cherokee
    , unifiedCanadianAboriginalSyllabics
    , ogham
    , runic
    , tagalog
    , hanunoo
    , buhid
    , tagbanwa
    , khmer
    , mongolian
    , limbu
    , taiLe
    , khmerSymbols
    , phoneticExtensions
    , latinExtendedAdditional
    , greekExtended
    , generalPunctuation
    , superscriptsAndSubscripts
    , currencySymbols
    , combiningDiacriticalMarksForSymbols
    , letterlikeSymbols
    , numberForms
    , arrows
    , mathematicalOperators
    , miscellaneousTechnical
    , controlPictures
    , opticalCharacterRecognition
    , enclosedAlphanumerics
    , boxDrawing
    , blockElements
    , geometricShapes
    , miscellaneousSymbols
    , dingbats
    , miscellaneousMathematicalSymbolsA
    , supplementalArrowsA
    , braillePatterns
    , supplementalArrowsB
    , miscellaneousMathematicalSymbolsB
    , supplementalMathematicalOperators
    , miscellaneousSymbolsAndArrows
    , cjkRadicalsSupplement
    , kangxiRadicals
    , ideographicDescriptionCharacters
    , cjkSymbolsAndPunctuation
    , hiragana
    , katakana
    , bopomofo
    , hangulCompatibilityJamo
    , kanbun
    , bopomofoExtended
    , katakanaPhoneticExtensions
    , enclosedCjkLettersAndMonths
    , cjkCompatibility
    , cjkUnifiedIdeographsExtensionA
    , yijingHexagramSymbols
    , cjkUnifiedIdeographs
    , yiSyllables
    , yiRadicals
    , hangulSyllables
    , highSurrogates
    , highPrivateUseSurrogates
    , lowSurrogates
    , privateUseArea
    , cjkCompatibilityIdeographs
    , alphabeticPresentationForms
    , arabicPresentationFormsA
    , variationSelectors
    , combiningHalfMarks
    , cjkCompatibilityForms
    , smallFormVariants
    , arabicPresentationFormsB
    , halfwidthAndFullwidthForms
    , specials
    ) where

import Data.Char
import Text.Trifecta.CharSet
import Data.Data
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap

data Block = Block 
    { blockName :: String
    , blockCharSet :: CharSet
    } deriving (Show, Data, Typeable)

blocks :: [Block]
blocks =
    [ Block "Basic_Latin" basicLatin
    , Block "Latin-1_Supplement" latin1Supplement
    , Block "Latin_Extended-A" latinExtendedA
    , Block "IPA_Extensions" ipaExtensions
    , Block "Spacing_Modifier_Letters" spacingModifierLetters

    , Block "Latin_Extended-A" latinExtendedA
    , Block "Latin_Extended-B" latinExtendedB
    , Block "IPA_Extensions" ipaExtensions
    , Block "Spacing_Modifier_Letters" spacingModifierLetters
    , Block "Combining_Diacritical_Marks" combiningDiacriticalMarks
    , Block "Greek_and_Coptic" greekAndCoptic
    , Block "Cyrillic" cyrillic
    , Block "Cyrillic_Supplementary" cyrillicSupplementary
    , Block "Armenian" armenian
    , Block "Hebrew" hebrew
    , Block "Arabic" arabic
    , Block "Syriac" syriac
    , Block "Thaana" thaana
    , Block "Devanagari" devanagari
    , Block "Bengali" bengali
    , Block "Gurmukhi" gurmukhi
    , Block "Gujarati" gujarati
    , Block "Oriya" oriya
    , Block "Tamil" tamil
    , Block "Telugu" telugu
    , Block "Kannada" kannada
    , Block "Malayalam" malayalam
    , Block "Sinhala" sinhala
    , Block "Thai" thai
    , Block "Lao" lao
    , Block "Tibetan" tibetan
    , Block "Myanmar" myanmar
    , Block "Georgian" georgian
    , Block "Hangul_Jamo" hangulJamo
    , Block "Ethiopic" ethiopic
    , Block "Cherokee" cherokee
    , Block "Unified_Canadian_Aboriginal_Syllabics" unifiedCanadianAboriginalSyllabics
    , Block "Ogham" ogham
    , Block "Runic" runic
    , Block "Tagalog" tagalog
    , Block "Hanunoo" hanunoo
    , Block "Buhid" buhid
    , Block "Tagbanwa" tagbanwa
    , Block "Khmer" khmer
    , Block "Mongolian" mongolian
    , Block "Limbu" limbu
    , Block "Tai_Le" taiLe
    , Block "Khmer_Symbols" khmerSymbols
    , Block "Phonetic_Extensions" phoneticExtensions
    , Block "Latin_Extended_Additional" latinExtendedAdditional
    , Block "Greek_Extended" greekExtended
    , Block "General_Punctuation" generalPunctuation
    , Block "Superscripts_and_Subscripts" superscriptsAndSubscripts
    , Block "Currency_Symbols" currencySymbols
    , Block "Combining_Diacritical_Marks_for_Symbols" combiningDiacriticalMarksForSymbols
    , Block "Letterlike_Symbols" letterlikeSymbols
    , Block "Number_Forms" numberForms
    , Block "Arrows" arrows
    , Block "Mathematical_Operators" mathematicalOperators
    , Block "Miscellaneous_Technical" miscellaneousTechnical
    , Block "Control_Pictures" controlPictures
    , Block "Optical_Character_Recognition" opticalCharacterRecognition
    , Block "Enclosed_Alphanumerics" enclosedAlphanumerics
    , Block "Box_Drawing" boxDrawing
    , Block "Block_Elements" blockElements
    , Block "Geometric_Shapes" geometricShapes
    , Block "Miscellaneous_Symbols" miscellaneousSymbols
    , Block "Dingbats" dingbats
    , Block "Miscellaneous_Mathematical_Symbols-A" miscellaneousMathematicalSymbolsA
    , Block "Supplemental_Arrows-A" supplementalArrowsA
    , Block "Braille_Patterns" braillePatterns
    , Block "Supplemental_Arrows-B" supplementalArrowsB
    , Block "Miscellaneous_Mathematical_Symbols-B" miscellaneousMathematicalSymbolsB
    , Block "Supplemental_Mathematical_Operators" supplementalMathematicalOperators
    , Block "Miscellaneous_Symbols_and_Arrows" miscellaneousSymbolsAndArrows
    , Block "CJK_Radicals_Supplement" cjkRadicalsSupplement
    , Block "Kangxi_Radicals" kangxiRadicals
    , Block "Ideographic_Description_Characters" ideographicDescriptionCharacters
    , Block "CJK_Symbols_and_Punctuation" cjkSymbolsAndPunctuation
    , Block "Hiragana" hiragana
    , Block "Katakana" katakana
    , Block "Bopomofo" bopomofo
    , Block "Hangul_Compatibility_Jamo" hangulCompatibilityJamo
    , Block "Kanbun" kanbun
    , Block "Bopomofo_Extended" bopomofoExtended
    , Block "Katakana_Phonetic_Extensions" katakanaPhoneticExtensions
    , Block "Enclosed_CJK_Letters_and_Months" enclosedCjkLettersAndMonths
    , Block "CJK_Compatibility" cjkCompatibility
    , Block "CJK_Unified_Ideographs_Extension_A" cjkUnifiedIdeographsExtensionA
    , Block "Yijing_Hexagram_Symbols" yijingHexagramSymbols
    , Block "CJK_Unified_Ideographs" cjkUnifiedIdeographs
    , Block "Yi_Syllables" yiSyllables
    , Block "Yi_Radicals" yiRadicals
    , Block "Hangul_Syllables" hangulSyllables
    , Block "High_Surrogates" highSurrogates
    , Block "High_Private_Use_Surrogates" highPrivateUseSurrogates
    , Block "Low_Surrogates" lowSurrogates
    , Block "Private_Use_Area" privateUseArea
    , Block "CJK_Compatibility_Ideographs" cjkCompatibilityIdeographs
    , Block "Alphabetic_Presentation_Forms" alphabeticPresentationForms
    , Block "Arabic_Presentation_Forms-A" arabicPresentationFormsA
    , Block "Variation_Selectors" variationSelectors
    , Block "Combining_Half_Marks" combiningHalfMarks
    , Block "CJK_Compatibility_Forms" cjkCompatibilityForms
    , Block "Small_Form_Variants" smallFormVariants
    , Block "Arabic_Presentation_Forms-B" arabicPresentationFormsB
    , Block "Halfwidth_and_Fullwidth_Forms" halfwidthAndFullwidthForms
    , Block "Specials" specials ]

lookupTable :: HashMap String Block
lookupTable = HashMap.fromList $ 
              Prelude.map (\y@(Block x _) -> (canonicalize x, y))
              blocks

canonicalize :: String -> String
canonicalize s = case Prelude.map toLower s of
    'i': 'n' : xs -> go xs
    xs -> go xs
    where
        go ('-':xs) = go xs
        go ('_':xs) = go xs
        go (' ':xs) = go xs
        go (x:xs) = x : go xs
        go [] = []

lookupBlock :: String -> Maybe Block
lookupBlock s = HashMap.lookup (canonicalize s) lookupTable

lookupBlockCharSet :: String -> Maybe CharSet
lookupBlockCharSet = fmap blockCharSet . lookupBlock

basicLatin = range '\x0000' '\x007f'
latin1Supplement = range '\x0080' '\x00ff'
latinExtendedA = range '\x0100' '\x017F'
latinExtendedB = range '\x0180' '\x024F'
ipaExtensions = range '\x0250' '\x02AF'
spacingModifierLetters = range '\x02B0' '\x02FF'
combiningDiacriticalMarks = range '\x0300' '\x036F'
greekAndCoptic = range '\x0370' '\x03FF'
cyrillic = range '\x0400' '\x04FF'
cyrillicSupplementary = range '\x0500' '\x052F'
armenian = range '\x0530' '\x058F'
hebrew = range '\x0590' '\x05FF'
arabic = range '\x0600' '\x06FF'
syriac = range '\x0700' '\x074F'
thaana = range '\x0780' '\x07BF'
devanagari = range '\x0900' '\x097F'
bengali = range '\x0980' '\x09FF'
gurmukhi = range '\x0A00' '\x0A7F'
gujarati = range '\x0A80' '\x0AFF'
oriya = range '\x0B00' '\x0B7F'
tamil = range '\x0B80' '\x0BFF'
telugu = range '\x0C00' '\x0C7F'
kannada = range '\x0C80' '\x0CFF'
malayalam = range '\x0D00' '\x0D7F'
sinhala = range '\x0D80' '\x0DFF'
thai = range '\x0E00' '\x0E7F'
lao = range '\x0E80' '\x0EFF'
tibetan = range '\x0F00' '\x0FFF'
myanmar = range '\x1000' '\x109F'
georgian = range '\x10A0' '\x10FF'
hangulJamo = range '\x1100' '\x11FF'
ethiopic = range '\x1200' '\x137F'
cherokee = range '\x13A0' '\x13FF'
unifiedCanadianAboriginalSyllabics = range '\x1400' '\x167F'
ogham = range '\x1680' '\x169F'
runic = range '\x16A0' '\x16FF'
tagalog = range '\x1700' '\x171F'
hanunoo = range '\x1720' '\x173F'
buhid = range '\x1740' '\x175F'
tagbanwa = range '\x1760' '\x177F'
khmer = range '\x1780' '\x17FF'
mongolian = range '\x1800' '\x18AF'
limbu = range '\x1900' '\x194F'
taiLe = range '\x1950' '\x197F'
khmerSymbols = range '\x19E0' '\x19FF'
phoneticExtensions = range '\x1D00' '\x1D7F'
latinExtendedAdditional = range '\x1E00' '\x1EFF'
greekExtended = range '\x1F00' '\x1FFF'
generalPunctuation = range '\x2000' '\x206F'
superscriptsAndSubscripts = range '\x2070' '\x209F'
currencySymbols = range '\x20A0' '\x20CF'
combiningDiacriticalMarksForSymbols = range '\x20D0' '\x20FF'
letterlikeSymbols = range '\x2100' '\x214F'
numberForms = range '\x2150' '\x218F'
arrows = range '\x2190' '\x21FF'
mathematicalOperators = range '\x2200' '\x22FF'
miscellaneousTechnical = range '\x2300' '\x23FF'
controlPictures = range '\x2400' '\x243F'
opticalCharacterRecognition = range '\x2440' '\x245F'
enclosedAlphanumerics = range '\x2460' '\x24FF'
boxDrawing = range '\x2500' '\x257F'
blockElements = range '\x2580' '\x259F'
geometricShapes = range '\x25A0' '\x25FF'
miscellaneousSymbols = range '\x2600' '\x26FF'
dingbats = range '\x2700' '\x27BF'
miscellaneousMathematicalSymbolsA = range '\x27C0' '\x27EF'
supplementalArrowsA = range '\x27F0' '\x27FF'
braillePatterns = range '\x2800' '\x28FF'
supplementalArrowsB = range '\x2900' '\x297F'
miscellaneousMathematicalSymbolsB = range '\x2980' '\x29FF'
supplementalMathematicalOperators = range '\x2A00' '\x2AFF'
miscellaneousSymbolsAndArrows = range '\x2B00' '\x2BFF'
cjkRadicalsSupplement = range '\x2E80' '\x2EFF'
kangxiRadicals = range '\x2F00' '\x2FDF'
ideographicDescriptionCharacters = range '\x2FF0' '\x2FFF'
cjkSymbolsAndPunctuation = range '\x3000' '\x303F'
hiragana = range '\x3040' '\x309F'
katakana = range '\x30A0' '\x30FF'
bopomofo = range '\x3100' '\x312F'
hangulCompatibilityJamo = range '\x3130' '\x318F'
kanbun = range '\x3190' '\x319F'
bopomofoExtended = range '\x31A0' '\x31BF'
katakanaPhoneticExtensions = range '\x31F0' '\x31FF'
enclosedCjkLettersAndMonths = range '\x3200' '\x32FF'
cjkCompatibility = range '\x3300' '\x33FF'
cjkUnifiedIdeographsExtensionA = range '\x3400' '\x4DBF'
yijingHexagramSymbols = range '\x4DC0' '\x4DFF'
cjkUnifiedIdeographs = range '\x4E00' '\x9FFF'
yiSyllables = range '\xA000' '\xA48F'
yiRadicals = range '\xA490' '\xA4CF'
hangulSyllables = range '\xAC00' '\xD7AF'
highSurrogates = range '\xD800' '\xDB7F'
highPrivateUseSurrogates = range '\xDB80' '\xDBFF'
lowSurrogates = range '\xDC00' '\xDFFF'
privateUseArea = range '\xE000' '\xF8FF'
cjkCompatibilityIdeographs = range '\xF900' '\xFAFF'
alphabeticPresentationForms = range '\xFB00' '\xFB4F'
arabicPresentationFormsA = range '\xFB50' '\xFDFF'
variationSelectors = range '\xFE00' '\xFE0F'
combiningHalfMarks = range '\xFE20' '\xFE2F'
cjkCompatibilityForms = range '\xFE30' '\xFE4F'
smallFormVariants = range '\xFE50' '\xFE6F'
arabicPresentationFormsB = range '\xFE70' '\xFEFF'
halfwidthAndFullwidthForms = range '\xFF00' '\xFFEF'
specials = range '\xFFF0' '\xFFFF'

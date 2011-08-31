{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, UndecidableInstances, FlexibleInstances, FlexibleContexts, PatternGuards #-}
{-# OPTIONS_GHC -fspec-constr -fspec-constr-count=8 #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Trifecta.Parser.Char
-- Copyright   :  (C) 2011 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
----------------------------------------------------------------------------
module Text.Trifecta.Parser.Char
  ( oneOf      -- :: MonadParser m => [Char] -> m Char
  , noneOf     -- :: MonadParser m => [Char] -> m Char
  , oneOfSet   -- :: MonadParser m => CharSet -> m Char
  , noneOfSet  -- :: MonadParser m => CharSet -> m Char
  , spaces     -- :: MonadParser m => m ()
  , space      -- :: MonadParser m => m Char
  , newline    -- :: MonadParser m => m Char
  , tab        -- :: MonadParser m => m Char
  , upper      -- :: MonadParser m => m Char
  , lower      -- :: MonadParser m => m Char
  , alphaNum   -- :: MonadParser m => m Char
  , letter     -- :: MonadParser m => m Char
  , digit      -- :: MonadParser m => m Char
  , hexDigit   -- :: MonadParser m => m Char
  , octDigit   -- :: MonadParser m => m Char
  , char       -- :: MonadParser m => Char -> m Char
  , notChar    -- :: MonadParser m => Char -> m Char
  , anyChar    -- :: MonadParser m => m Char
  , string     -- :: MonadParser m => String -> m String
  , byteString -- :: MonadParser m => ByteString -> m ByteString
  ) where

import Data.Char
import Control.Applicative
import Control.Monad (guard)
import Text.Trifecta.Parser.Class
import Text.Trifecta.Rope.Delta
import qualified Data.IntSet as IntSet
import Text.Trifecta.CharSet (CharSet(..))
import qualified Text.Trifecta.CharSet as CharSet
import qualified Text.Trifecta.ByteSet as ByteSet
import qualified Data.ByteString as Strict
import Data.ByteString.Internal (w2c,c2w)
import Data.ByteString.UTF8 as UTF8

-- | @oneOf cs@ succeeds if the current character is in the supplied
-- list of characters @cs@. Returns the parsed character. See also
-- 'satisfy'.
-- 
-- >   vowel  = oneOf "aeiou"
oneOf :: MonadParser m => [Char] -> m Char
oneOf xs = oneOfSet (CharSet.fromList xs)
{-# INLINE oneOf #-}

-- | As the dual of 'oneOf', @noneOf cs@ succeeds if the current
-- character /not/ in the supplied list of characters @cs@. Returns the
-- parsed character.
--
-- >  consonant = noneOf "aeiou"
noneOf :: MonadParser m => [Char] -> m Char
noneOf xs = noneOfSet (CharSet.fromList xs)
{-# INLINE noneOf #-}

-- | @oneOfSet cs@ succeeds if the current character is in the supplied
-- set of characters @cs@. Returns the parsed character. See also
-- 'satisfy'.
-- 
-- >   vowel  = oneOf "aeiou"
oneOfSet :: MonadParser m => CharSet -> m Char
oneOfSet (CharSet True bs is)
  | (_,r) <- IntSet.split 0x80 is, IntSet.null r = w2c <$> satisfy8 (\w -> ByteSet.member w bs)
  | otherwise                                    = satisfy  (\c -> IntSet.member (fromEnum c) is)
oneOfSet (CharSet False bs is) 
  | (_,r) <- IntSet.split 0x80 is, not (IntSet.null r) = satisfy (\c -> not (IntSet.member (fromEnum c) is))
  | otherwise                                          = satisfyAscii (\c -> not (ByteSet.member (c2w c) bs))
{-# INLINE oneOfSet #-}
  
-- | As the dual of 'oneOf', @noneOf cs@ succeeds if the current
-- character /not/ in the supplied list of characters @cs@. Returns the
-- parsed character.
--
-- >  consonant = noneOf "aeiou"
noneOfSet :: MonadParser m => CharSet -> m Char
noneOfSet s = oneOfSet (CharSet.complement s)
{-# INLINE noneOfSet #-}

-- | Skips /zero/ or more white space characters. See also 'skipMany' and
-- 'whiteSpace'.
spaces :: MonadParser m => m ()
spaces = skipMany space <?> "white space"

-- | Parses a white space character (any character which satisfies 'isSpace')
-- Returns the parsed character. 
space :: MonadParser m => m Char
space = satisfy isSpace <?> "space"
{-# INLINE space #-}

-- | Parses a newline character (\'\\n\'). Returns a newline character. 
newline :: MonadParser m => m Char
newline = char '\n' <?> "new-line"
{-# INLINE newline #-}

-- | Parses a tab character (\'\\t\'). Returns a tab character. 
tab :: MonadParser m => m Char
tab = char '\t' <?> "tab"
{-# INLINE tab #-}

-- | Parses an upper case letter (a character between \'A\' and \'Z\').
-- Returns the parsed character. 
upper :: MonadParser m => m Char
upper = satisfy isUpper <?> "uppercase letter"
{-# INLINE upper #-}

-- | Parses a lower case character (a character between \'a\' and \'z\').
-- Returns the parsed character. 
lower :: MonadParser m => m Char
lower = satisfy isLower <?> "lowercase letter"
{-# INLINE lower #-}

-- | Parses a letter or digit (a character between \'0\' and \'9\').
-- Returns the parsed character. 

alphaNum :: MonadParser m => m Char
alphaNum = satisfy isAlphaNum <?> "letter or digit"
{-# INLINE alphaNum #-}

-- | Parses a letter (an upper case or lower case character). Returns the
-- parsed character. 

letter :: MonadParser m => m Char
letter = satisfy isAlpha <?> "letter"
{-# INLINE letter #-}

-- | Parses a digit. Returns the parsed character. 

digit :: MonadParser m => m Char
digit = satisfy isDigit <?> "digit"
{-# INLINE digit #-}

-- | Parses a hexadecimal digit (a digit or a letter between \'a\' and
-- \'f\' or \'A\' and \'F\'). Returns the parsed character. 
hexDigit :: MonadParser m => m Char
hexDigit = satisfy isHexDigit <?> "hexadecimal digit"
{-# INLINE hexDigit #-}

-- | Parses an octal digit (a character between \'0\' and \'7\'). Returns
-- the parsed character. 
octDigit :: MonadParser m => m Char
octDigit = satisfy isOctDigit <?> "octal digit"
{-# INLINE octDigit #-}

-- | @char c@ parses a single character @c@. Returns the parsed
-- character (i.e. @c@).
--
-- >  semiColon  = char ';'
char :: MonadParser m => Char -> m Char
char c 
  | c <= w2c 0x7f, w <- c2w c = w2c <$> satisfy8 (w ==) <?> show [c]
  | otherwise                 = satisfy (c ==) <?> show [c]
{-# INLINE char #-}

-- | @notChar c@ parses any single character other than @c@. Returns the parsed
-- character.
--
-- >  semiColon  = char ';'
notChar :: MonadParser m => Char -> m Char
notChar c 
  | fromEnum c <= 0x7f = satisfyAscii (c /=)
  | otherwise          = satisfy (c /=)
{-# INLINE notChar #-}

-- | This parser succeeds for any character. Returns the parsed character. 
anyChar :: MonadParser m => m Char
anyChar = satisfy (const True)

-- | @string s@ parses a sequence of characters given by @s@. Returns
-- the parsed string (i.e. @s@).
--
-- >  divOrMod    =   string "div" 
-- >              <|> string "mod"
string :: MonadParser m => String -> m String
string s = s <$ byteString (UTF8.fromString s) <?> show s

-- | @byteString s@ parses a sequence of bytes given by @s@. Returns
-- the parsed byteString (i.e. @s@).
--
-- >  divOrMod    =   string "div" 
-- >              <|> string "mod"
byteString :: MonadParser m => Strict.ByteString -> m Strict.ByteString
byteString bs = do
   r <- restOfLine
   let lr = Strict.length r
       lbs = Strict.length bs
   guard $ lr > 0
   case compare lbs lr of
     LT | bs `Strict.isPrefixOf` r -> bs <$ skipping (delta bs)
     EQ | bs == r -> bs <$ skipping (delta bs)
     GT | r `Strict.isPrefixOf` bs -> bs <$ skipping (delta r) *> byteString (Strict.drop lr bs)
     _ -> empty 
 <?> show (UTF8.toString bs)

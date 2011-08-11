{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, UndecidableInstances, FlexibleInstances, FlexibleContexts #-}
module Text.Trifecta.Parser.Char
  ( oneOf      -- :: MonadParser m => [Char] -> m Char
  , noneOf     -- :: MonadParser m => [Char] -> m Char
--  , spaces     -- :: MonadParser m => m ()
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
  , anyChar    -- :: MonadParser m => m Char
  , string     -- :: MonadParser m => String -> m String
  , byteString -- :: MonadParser m => ByteString -> m ByteString
  ) where

import Data.Char
import Control.Applicative
import Control.Monad (guard)
import Text.Trifecta.Parser.Class
import Data.ByteString as Strict hiding (empty, all, elem)
import Data.ByteString.UTF8 as UTF8

-- | @oneOf cs@ succeeds if the current character is in the supplied
-- list of characters @cs@. Returns the parsed character. See also
-- 'satisfy'.
-- 
-- >   vowel  = oneOf "aeiou"
oneOf :: MonadParser m => [Char] -> m Char
oneOf cs | all ((< 0x80) . fromEnum) cs = satisfyAscii (\c -> elem c cs)
         | otherwise                    = satisfy (\c -> elem c cs)

-- | As the dual of 'oneOf', @noneOf cs@ succeeds if the current
-- character /not/ in the supplied list of characters @cs@. Returns the
-- parsed character.
--
-- >  consonant = noneOf "aeiou"
noneOf :: MonadParser m => [Char] -> m Char
noneOf cs | all ((< 0x80) . fromEnum) cs = satisfyAscii (\c -> not (elem c cs))
          | otherwise                    = satisfy (\c -> not (elem c cs))

-- | Skips /zero/ or more white space characters. See also 'skipMany'.
-- spaces :: MonadParser m => m ()
-- spaces = skipMany space <?> "white space"

-- | Parses a white space character (any character which satisfies 'isSpace')
-- Returns the parsed character. 
space :: MonadParser m => m Char
space = satisfy isSpace <?> "space"

-- | Parses a newline character (\'\\n\'). Returns a newline character. 
newline :: MonadParser m => m Char
newline = char '\n' <?> "new-line"

-- | Parses a tab character (\'\\t\'). Returns a tab character. 
tab :: MonadParser m => m Char
tab = char '\t' <?> "tab"

-- | Parses an upper case letter (a character between \'A\' and \'Z\').
-- Returns the parsed character. 
upper :: MonadParser m => m Char
upper = satisfy isUpper <?> "uppercase letter"

-- | Parses a lower case character (a character between \'a\' and \'z\').
-- Returns the parsed character. 
lower :: MonadParser m => m Char
lower = satisfy isLower <?> "lowercase letter"

-- | Parses a letter or digit (a character between \'0\' and \'9\').
-- Returns the parsed character. 

alphaNum :: MonadParser m => m Char
alphaNum = satisfy isAlphaNum <?> "letter or digit"

-- | Parses a letter (an upper case or lower case character). Returns the
-- parsed character. 

letter :: MonadParser m => m Char
letter = satisfy isAlpha <?> "letter"

-- | Parses a digit. Returns the parsed character. 

digit :: MonadParser m => m Char
digit = satisfy isDigit <?> "digit"

-- | Parses a hexadecimal digit (a digit or a letter between \'a\' and
-- \'f\' or \'A\' and \'F\'). Returns the parsed character. 
hexDigit :: MonadParser m => m Char
hexDigit = satisfy isHexDigit <?> "hexadecimal digit"

-- | Parses an octal digit (a character between \'0\' and \'7\'). Returns
-- the parsed character. 
octDigit :: MonadParser m => m Char
octDigit = satisfy isOctDigit    <?> "octal digit"

-- | @char c@ parses a single character @c@. Returns the parsed
-- character (i.e. @c@).
--
-- >  semiColon  = char ';'
char :: MonadParser m => Char -> m Char
char c | fromEnum c <= 0x7f = satisfyAscii (c ==) <?> show [c]
       | otherwise          = satisfy (c ==) <?> show [c]

-- | This parser succeeds for any character. Returns the parsed character. 
anyChar :: MonadParser m => m Char
anyChar = satisfy (const True)

-- | @string s@ parses a sequence of characters given by @s@. Returns
-- the parsed string (i.e. @s@).
--
-- >  divOrMod    =   string "div" 
-- >              <|> string "mod"
string :: MonadParser m => String -> m String
string s = s <$ byteString (UTF8.fromString s)

-- | @byteString s@ parses a sequence of bytes given by @s@. Returns
-- the parsed byteString (i.e. @s@).
--
-- >  divOrMod    =   string "div" 
-- >              <|> string "mod"
byteString :: MonadParser m => ByteString -> m ByteString
byteString bs = do
   r <- restOfLine
   let lr = Strict.length r
       lbs = Strict.length bs
   guard $ lr > 0
   case compare lbs lr of
     LT | bs `isPrefixOf` r -> bs <$ skipping bs
        | otherwise -> empty
     EQ | bs == r -> bs <$ skipping bs
        | otherwise -> empty
     GT | r `isPrefixOf` bs -> bs <$ skipping r *> byteString (Strict.drop lr bs)
        | otherwise -> empty
 <?> UTF8.toString bs

-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Trifecta.Parser.Token.Combinators
-- Copyright   :  (c) Edward Kmett 2011,
--                (c) Daan Leijen 1999-2001
-- License     :  BSD3
-- 
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  provisional
-- Portability :  non-portable
-- 
-----------------------------------------------------------------------------
module Text.Trifecta.Parser.Token.Combinators
  ( lexeme
  , charLiteral
  , stringLiteral
  , natural
  , integer
  , double
  , naturalOrDouble
  , symbol
  , symbolic
  , parens
  , braces
  , angles
  , brackets
  , comma
  , colon
  , dot
  , semiSep
  , semiSep1
  , commaSep
  , commaSep1
  ) where

import Data.ByteString as Strict hiding (map, zip, foldl, foldr)
import Control.Applicative
import Text.Trifecta.Parser.Class
import Text.Trifecta.Parser.Char
import Text.Trifecta.Parser.Combinators
import Text.Trifecta.Parser.Token.Prim
import Text.Trifecta.Highlight.Prim

-- | @lexeme p@ first applies parser @p@ and then the 'whiteSpace'
-- parser, returning the value of @p@. Every lexical
-- token (lexeme) is defined using @lexeme@, this way every parse
-- starts at a point without white space. Parsers that use @lexeme@ are
-- called /lexeme/ parsers in this document.
--
-- The only point where the 'whiteSpace' parser should be
-- called explicitly is the start of the main parser in order to skip
-- any leading white space.
--
-- >    mainParser  = do { whiteSpace
-- >                     ; ds <- many (lexeme digit)
-- >                     ; eof
-- >                     ; return (sum ds)
-- >                     }
lexeme :: MonadParser m => m a -> m a
lexeme p = p <* whiteSpace

-- | This lexeme parser parses a single literal character. Returns the
-- literal character value. This parsers deals correctly with escape
-- sequences. The literal character is parsed according to the grammar
-- rules defined in the Haskell report (which matches most programming
-- languages quite closely). 
charLiteral :: MonadParser m => m Char
charLiteral = lexeme charLiteral'

-- | This lexeme parser parses a literal string. Returns the literal
-- string value. This parsers deals correctly with escape sequences and
-- gaps. The literal string is parsed according to the grammar rules
-- defined in the Haskell report (which matches most programming
-- languages quite closely). 

stringLiteral :: MonadParser m => m String
stringLiteral = lexeme stringLiteral'

-- | This lexeme parser parses a natural number (a positive whole
-- number). Returns the value of the number. The number can be
-- specified in 'decimal', 'hexadecimal' or
-- 'octal'. The number is parsed according to the grammar
-- rules in the Haskell report. 

natural :: MonadParser m => m Integer
natural = lexeme natural'

-- | This lexeme parser parses an integer (a whole number). This parser
-- is like 'natural' except that it can be prefixed with
-- sign (i.e. \'-\' or \'+\'). Returns the value of the number. The
-- number can be specified in 'decimal', 'hexadecimal'
-- or 'octal'. The number is parsed according
-- to the grammar rules in the Haskell report. 

integer :: MonadParser m => m Integer
integer = lexeme int <?> "integer"
  where
  sign = negate <$ char '-'
    <|> id <$ char '+'
    <|> pure id
  int = lexeme (highlight Operator sign) <*> natural'

-- | This lexeme parser parses a floating point value. Returns the value
-- of the number. The number is parsed according to the grammar rules
-- defined in the Haskell report. 

double :: MonadParser m => m Double
double = lexeme double'

-- | This lexeme parser parses either 'natural' or a 'float'.
-- Returns the value of the number. This parsers deals with
-- any overlap in the grammar rules for naturals and floats. The number
-- is parsed according to the grammar rules defined in the Haskell report. 

naturalOrDouble :: MonadParser m => m (Either Integer Double)
naturalOrDouble = lexeme naturalOrDouble'

-- | Lexeme parser @symbol s@ parses 'string' @s@ and skips
-- trailing white space. 

symbol :: MonadParser m => ByteString -> m ByteString
symbol name = lexeme (highlight Symbol (byteString name))

-- | Lexeme parser @symbolic s@ parses 'char' @s@ and skips
-- trailing white space. 

symbolic :: MonadParser m => Char -> m Char
symbolic name = lexeme (highlight Symbol (char name))

-- | Lexeme parser @parens p@ parses @p@ enclosed in parenthesis,
-- returning the value of @p@.

parens :: MonadParser m => m a -> m a
parens = nesting . between (symbolic '(') (symbolic ')')

-- | Lexeme parser @braces p@ parses @p@ enclosed in braces (\'{\' and
-- \'}\'), returning the value of @p@. 

braces :: MonadParser m => m a -> m a
braces = nesting . between (symbolic '{') (symbolic '}')

-- | Lexeme parser @angles p@ parses @p@ enclosed in angle brackets (\'\<\'
-- and \'>\'), returning the value of @p@. 

angles :: MonadParser m => m a -> m a
angles = nesting . between (symbolic '<') (symbolic '>')

-- | Lexeme parser @brackets p@ parses @p@ enclosed in brackets (\'[\'
-- and \']\'), returning the value of @p@. 

brackets :: MonadParser m => m a -> m a
brackets = nesting . between (symbolic '[') (symbolic ']')

-- | Lexeme parser @comma@ parses the character \',\' and skips any
-- trailing white space. Returns the string \",\". 

comma :: MonadParser m => m Char
comma = symbolic ','

-- | Lexeme parser @colon@ parses the character \':\' and skips any
-- trailing white space. Returns the string \":\". 

colon :: MonadParser m => m Char
colon = symbolic ':'

-- | Lexeme parser @dot@ parses the character \'.\' and skips any
-- trailing white space. Returns the string \".\". 

dot :: MonadParser m => m Char
dot = symbolic '.'

-- | Lexeme parser @semiSep p@ parses /zero/ or more occurrences of @p@
-- separated by 'semi'. Returns a list of values returned by
-- @p@.

semiSep :: MonadParser m => m a -> m [a]
semiSep p = sepBy p semi

-- | Lexeme parser @semiSep1 p@ parses /one/ or more occurrences of @p@
-- separated by 'semi'. Returns a list of values returned by @p@. 

semiSep1 :: MonadParser m => m a -> m [a]
semiSep1 p = sepBy1 p semi

-- | Lexeme parser @commaSep p@ parses /zero/ or more occurrences of
-- @p@ separated by 'comma'. Returns a list of values returned
-- by @p@. 

commaSep :: MonadParser m => m a -> m [a]
commaSep p = sepBy p comma

-- | Lexeme parser @commaSep1 p@ parses /one/ or more occurrences of
-- @p@ separated by 'comma'. Returns a list of values returned
-- by @p@. 

commaSep1 :: MonadParser m => m a -> m [a]
commaSep1 p = sepBy p comma

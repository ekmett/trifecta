-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Trifecta.Parser.Token
-- Copyright   :  (c) Edward Kmett 2011,
--                (c) Daan Leijen 1999-2001
-- License     :  BSD3
-- 
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  provisional
-- Portability :  non-portable
-- 
-----------------------------------------------------------------------------
module Text.Trifecta.Parser.Token
  ( identifier
  , reserved
  , operator
  , reservedOp
  , charLiteral
  , stringLiteral
  , natural
  , integer
  , double
  , naturalOrDouble
  , decimal
  , hexadecimal
  , octal
  , symbol
  , symbolic
  , lexeme
  , parens
  , braces
  , angles
  , brackets
  , semi
  , comma
  , colon
  , dot
  , semiSep
  , semiSep1
  , commaSep
  , commaSep1
  ) where

import Data.Char (digitToInt)
import Data.ByteString as Strict hiding (map, zip, foldl, foldr)
import Control.Applicative
import Control.Monad (when)
import Text.Trifecta.Parser.Class
import Text.Trifecta.Parser.Char
import Text.Trifecta.Parser.Combinators
import Text.Trifecta.Parser.Token.Class

-- | This lexeme parser parses a legal identifier. Returns the identifier
-- string. This parser will fail on identifiers that are reserved
-- words. Legal identifier (start) characters and reserved words are
-- defined in the 'LanguageDef' that is passed to
-- 'makeTokenParser'. An @identifier@ is treated as
-- a single token using 'try'.
identifier :: MonadTokenParser m => m ByteString
identifier = lexeme $ try $ do 
  name <- sliced ident 
  b <- isReservedName name
  when b $ unexpected $ "reserved word " ++ show name
  return name
  where 
    ident = identStart *> skipMany identLetter
        <?> "identifier"

-- | The lexeme parser @reserved name@ parses @symbol 
-- name@, but it also checks that the @name@ is not a prefix of a
-- valid identifier. A @reserved@ word is treated as a single token
-- using 'try'. 
reserved :: MonadTokenParser m => ByteString -> m ()
reserved name = lexeme $ try $ do
  _ <- byteString name
  notFollowedBy identLetter <?> "end of " ++ show name

-- | This lexeme parser parses a legal operator. Returns the name of the
-- operator. This parser will fail on any operators that are reserved
-- operators. Legal operator (start) characters and reserved operators
-- are defined in the 'LanguageDef' that is passed to
-- 'makeTokenParser'. An @operator@ is treated as a
-- single token using 'try'. 
operator :: MonadTokenParser m => m ByteString
operator = lexeme $ try $ do
  name <- sliced (opStart *> skipMany opLetter) <?> "operator"
  b <- isReservedOpName name
  when b $ unexpected $ "reserved operator " ++ show name
  return name

-- |The lexeme parser @reservedOp name@ parses @symbol
-- name@, but it also checks that the @name@ is not a prefix of a
-- valid operator. A @reservedOp@ is treated as a single token using
-- 'try'. 
reservedOp :: MonadTokenParser m => String -> m ()
reservedOp name = lexeme $ try $ do
   _ <- string name 
   notFollowedBy opLetter <?> "end of " ++ show name

-- | This lexeme parser parses a single literal character. Returns the
-- literal character value. This parsers deals correctly with escape
-- sequences. The literal character is parsed according to the grammar
-- rules defined in the Haskell report (which matches most programming
-- languages quite closely). 
charLiteral :: MonadTokenParser m => m Char
charLiteral = lexeme (between (char '\'') (char '\'' <?> "end of character") characterChar)
          <?> "character" 

characterChar, charEscape, charLetter :: MonadTokenParser m => m Char
characterChar = charLetter <|> charEscape
            <?> "literal character"
charEscape = char '\\' *> escapeCode
charLetter = satisfy (\c -> (c /= '\'') && (c /= '\\') && (c > '\026'))

-- | This lexeme parser parses a literal string. Returns the literal
-- string value. This parsers deals correctly with escape sequences and
-- gaps. The literal string is parsed according to the grammar rules
-- defined in the Haskell report (which matches most programming
-- languages quite closely). 

stringLiteral :: MonadTokenParser m => m String
stringLiteral = lexeme lit where
  lit = foldr (maybe id (:)) "" <$> between (char '"') (char '"' <?> "end of string") (many stringChar) 
    <?> "literal string"
  stringChar = Just <$> stringLetter 
           <|> stringEscape 
       <?> "string character"
  stringLetter    = satisfy (\c -> (c /= '"') && (c /= '\\') && (c > '\026'))

  stringEscape = char '\\' *> esc where
    esc = Nothing <$ escapeGap 
      <|> Nothing <$ escapeEmpty 
      <|> Just <$> escapeCode
  escapeEmpty = char '&'
  escapeGap = do skipSome space
                 char '\\' <?> "end of string gap"

escapeCode :: MonadTokenParser m => m Char
escapeCode = (charEsc <|> charNum <|> charAscii <|> charControl) <?> "escape code"
  where 
  charControl = (\c -> toEnum (fromEnum c - fromEnum 'A')) <$> (char '^' *> upper)
  charNum     = toEnum . fromInteger <$> num where
    num = decimal 
      <|> (char 'o' *> number 8 octDigit)
      <|> (char 'x' *> number 16 hexDigit)
  charEsc = choice $ parseEsc <$> escMap
  parseEsc (c,code) = code <$ char c
  escMap = zip ("abfnrtv\\\"\'") ("\a\b\f\n\r\t\v\\\"\'")
  charAscii = choice $ parseAscii <$> asciiMap
  parseAscii (asc,code) = try $ code <$ string asc
  asciiMap = zip (ascii3codes ++ ascii2codes) (ascii3 ++ ascii2)
  ascii2codes, ascii3codes :: [String]
  ascii2codes = [ "BS","HT","LF","VT","FF","CR","SO"
                , "SI","EM","FS","GS","RS","US","SP"]
  ascii3codes = ["NUL","SOH","STX","ETX","EOT","ENQ","ACK"
                ,"BEL","DLE","DC1","DC2","DC3","DC4","NAK"
                ,"SYN","ETB","CAN","SUB","ESC","DEL"]
  ascii2, ascii3 :: [Char]
  ascii2 = ['\BS','\HT','\LF','\VT','\FF','\CR','\SO','\SI'
           ,'\EM','\FS','\GS','\RS','\US','\SP']
  ascii3 = ['\NUL','\SOH','\STX','\ETX','\EOT','\ENQ','\ACK'
           ,'\BEL','\DLE','\DC1','\DC2','\DC3','\DC4','\NAK'
           ,'\SYN','\ETB','\CAN','\SUB','\ESC','\DEL']
  

-- | This lexeme parser parses a natural number (a positive whole
-- number). Returns the value of the number. The number can be
-- specified in 'decimal', 'hexadecimal' or
-- 'octal'. The number is parsed according to the grammar
-- rules in the Haskell report. 

natural :: MonadTokenParser m => m Integer
natural = lexeme nat <?> "natural"

number :: MonadTokenParser m => Integer -> m Char -> m Integer
number base baseDigit = do
  digits <- some baseDigit
  return $! foldl (\x d -> base*x + toInteger (digitToInt d)) 0 digits

-- | This lexeme parser parses an integer (a whole number). This parser
-- is like 'natural' except that it can be prefixed with
-- sign (i.e. \'-\' or \'+\'). Returns the value of the number. The
-- number can be specified in 'decimal', 'hexadecimal'
-- or 'octal'. The number is parsed according
-- to the grammar rules in the Haskell report. 
        
integer :: MonadTokenParser m => m Integer
integer = lexeme int <?> "integer"

sign :: MonadTokenParser m => m (Integer -> Integer)
sign = negate <$ char '-'
   <|> id <$ char '+'
   <|> pure id

nat, int, zeroNumber :: MonadTokenParser m => m Integer
nat = zeroNumber <|> decimal
int = lexeme sign <*> nat
zeroNumber = char '0' *> (hexadecimal <|> octal <|> decimal <|> return 0) <?> ""

-- | This lexeme parser parses a floating point value. Returns the value
-- of the number. The number is parsed according to the grammar rules
-- defined in the Haskell report. 

double :: MonadTokenParser m => m Double
double = lexeme floating <?> "double"

floating :: MonadTokenParser m => m Double
floating = decimal >>= fractExponent

fractExponent :: MonadTokenParser m => Integer -> m Double
fractExponent n = (\fract expo -> (fromInteger n + fract) * expo) <$> fraction <*> option 1.0 exponent'
              <|> (fromInteger n *) <$> exponent' where
  fraction = foldr op 0.0 <$> (char '.' *> (some digit <?> "fraction"))
  op d f = (f + fromIntegral (digitToInt d))/10.0
  exponent' = do
       _ <- oneOf "eE"
       f <- sign
       e <- decimal <?> "exponent"
       return (power (f e))
    <?> "exponent"
  power e  
    | e < 0     = 1.0/power(-e)
    | otherwise = fromInteger (10^e)

-- | This lexeme parser parses either 'natural' or a 'float'.
-- Returns the value of the number. This parsers deals with
-- any overlap in the grammar rules for naturals and floats. The number
-- is parsed according to the grammar rules defined in the Haskell report. 

naturalOrDouble :: MonadTokenParser m => m (Either Integer Double)
naturalOrDouble = lexeme natDouble <?> "number"

natDouble , zeroNumFloat, decimalFloat :: MonadTokenParser m => m (Either Integer Double)
natDouble 
    = char '0' *> zeroNumFloat
  <|> decimalFloat
zeroNumFloat
    = Left <$> (hexadecimal <|> octal)
  <|> decimalFloat
  <|> fractFloat 0
  <|> return (Left 0)
decimalFloat = do 
  n <- decimal
  option (Left n) (fractFloat n)

fractFloat :: MonadTokenParser m => Integer -> m (Either Integer Double)
fractFloat n = Right <$> fractExponent n

-- | Parses a positive whole number in the decimal system. Returns the
-- value of the number. 

decimal :: MonadTokenParser m => m Integer
decimal = number 10 digit

-- | Parses a positive whole number in the hexadecimal system. The number
-- should be prefixed with \"0x\" or \"0X\". Returns the value of the
-- number. 

hexadecimal :: MonadTokenParser m => m Integer
hexadecimal = oneOf "xX" *> number 16 hexDigit

-- | Parses a positive whole number in the octal system. The number
-- should be prefixed with \"0o\" or \"0O\". Returns the value of the
-- number. 

octal :: MonadTokenParser m => m Integer
octal = oneOf "oO" *> number 8 octDigit

-- | Lexeme parser @symbol s@ parses 'string' @s@ and skips
-- trailing white space. 

symbol :: MonadTokenParser m => ByteString -> m ByteString
symbol name = lexeme (byteString name)

-- | Lexeme parser @symbolic s@ parses 'char' @s@ and skips
-- trailing white space. 

symbolic :: MonadTokenParser m => Char -> m Char
symbolic name = lexeme (char name)

-- | @lexeme p@ first applies parser @p@ and than the 'whiteSpace'
-- parser, returning the value of @p@. Every lexical
-- token (lexeme) is defined using @lexeme@, this way every parse
-- starts at a point without white space. Parsers that use @lexeme@ are
-- called /lexeme/ parsers in this document.
-- 
-- The only point where the 'whiteSpace' parser should be
-- called explicitly is the start of the main parser in order to skip
-- any leading white space.
--
-- >    mainParser  = do{ whiteSpace
-- >                     ; ds <- many (lexeme digit)
-- >                     ; eof
-- >                     ; return (sum ds)
-- >                     }

lexeme :: MonadTokenParser m => m a -> m a
lexeme p = p <* whiteSpace

-- | Lexeme parser @parens p@ parses @p@ enclosed in parenthesis,
-- returning the value of @p@.

parens :: MonadTokenParser m => m a -> m a
parens = between (symbolic '(') (symbolic ')')


-- | Lexeme parser @braces p@ parses @p@ enclosed in braces (\'{\' and
-- \'}\'), returning the value of @p@. 

braces :: MonadTokenParser m => m a -> m a
braces = between (symbolic '{') (symbolic '}')

-- | Lexeme parser @angles p@ parses @p@ enclosed in angle brackets (\'\<\'
-- and \'>\'), returning the value of @p@. 

angles :: MonadTokenParser m => m a -> m a
angles = between (symbolic '<') (symbolic '>')

-- | Lexeme parser @brackets p@ parses @p@ enclosed in brackets (\'[\'
-- and \']\'), returning the value of @p@. 

brackets :: MonadTokenParser m => m a -> m a
brackets = between (symbolic '<') (symbolic '>')

-- | Lexeme parser |semi| parses the character \';\' and skips any
-- trailing white space. Returns the string \";\". 

semi :: MonadTokenParser m => m Char
semi = symbolic ';'

-- | Lexeme parser @comma@ parses the character \',\' and skips any
-- trailing white space. Returns the string \",\". 

comma :: MonadTokenParser m => m Char
comma = symbolic ','

-- | Lexeme parser @colon@ parses the character \':\' and skips any
-- trailing white space. Returns the string \":\". 

colon :: MonadTokenParser m => m Char
colon = symbolic ':'

-- | Lexeme parser @dot@ parses the character \'.\' and skips any
-- trailing white space. Returns the string \".\". 

dot :: MonadTokenParser m => m Char
dot = symbolic '.'

-- | Lexeme parser @semiSep p@ parses /zero/ or more occurrences of @p@
-- separated by 'semi'. Returns a list of values returned by
-- @p@.

semiSep :: MonadTokenParser m => m a -> m [a]
semiSep p = sepBy p semi

-- | Lexeme parser @semiSep1 p@ parses /one/ or more occurrences of @p@
-- separated by 'semi'. Returns a list of values returned by @p@. 

semiSep1 :: MonadTokenParser m => m a -> m [a]
semiSep1 p = sepBy1 p semi

-- | Lexeme parser @commaSep p@ parses /zero/ or more occurrences of
-- @p@ separated by 'comma'. Returns a list of values returned
-- by @p@. 

commaSep :: MonadTokenParser m => m a -> m [a]
commaSep p = sepBy p comma

-- | Lexeme parser @commaSep1 p@ parses /one/ or more occurrences of
-- @p@ separated by 'comma'. Returns a list of values returned
-- by @p@. 

commaSep1 :: MonadTokenParser m => m a -> m [a]
commaSep1 p = sepBy p comma

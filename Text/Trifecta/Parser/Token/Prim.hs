-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Trifecta.Parser.Token.Prim
-- Copyright   :  (c) Edward Kmett 2011,
--                (c) Daan Leijen 1999-2001
-- License     :  BSD3
-- 
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  provisional
-- Portability :  non-portable
-- 
-----------------------------------------------------------------------------
module Text.Trifecta.Parser.Token.Prim
  ( charLiteral'
  , characterChar
  , stringLiteral'
  , natural'
  , integer'
  , double'
  , naturalOrDouble'
  , decimal
  , hexadecimal
  , octal
  ) where

import Data.Char (digitToInt)
import Data.Foldable
import Control.Applicative
import Text.Trifecta.Parser.Class
import Text.Trifecta.Parser.Char
import Text.Trifecta.Parser.Combinators

-- | This parser parses a single literal character. Returns the
-- literal character value. This parsers deals correctly with escape
-- sequences. The literal character is parsed according to the grammar
-- rules defined in the Haskell report (which matches most programming
-- languages quite closely). 
--
-- This parser does NOT swallow trailing whitespace. 
charLiteral' :: MonadParser m => m Char
charLiteral' = between (char '\'') (char '\'' <?> "end of character") characterChar
          <?> "character" 

characterChar, charEscape, charLetter :: MonadParser m => m Char
characterChar = charLetter <|> charEscape
            <?> "literal character"
charEscape = char '\\' *> escapeCode
charLetter = satisfy (\c -> (c /= '\'') && (c /= '\\') && (c > '\026'))

-- | This parser parses a literal string. Returns the literal
-- string value. This parsers deals correctly with escape sequences and
-- gaps. The literal string is parsed according to the grammar rules
-- defined in the Haskell report (which matches most programming
-- languages quite closely). 
--
-- This parser does NOT swallow trailing whitespace
stringLiteral' :: MonadParser m => m String
stringLiteral' = lit where
  lit = Prelude.foldr (maybe id (:)) "" <$> between (char '"') (char '"' <?> "end of string") (many stringChar) 
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

escapeCode :: MonadParser m => m Char
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
--
-- This parser does NOT swallow trailing whitespace. 
natural' :: MonadParser m => m Integer
natural' = nat <?> "natural"

number :: MonadParser m => Integer -> m Char -> m Integer
number base baseDigit = do
  digits <- some baseDigit
  return $! foldl' (\x d -> base*x + toInteger (digitToInt d)) 0 digits

-- | This lexeme parser parses an integer (a whole number). This parser
-- is like 'natural' except that it can be prefixed with
-- sign (i.e. \'-\' or \'+\'). Returns the value of the number. The
-- number can be specified in 'decimal', 'hexadecimal'
-- or 'octal'. The number is parsed according
-- to the grammar rules in the Haskell report. 
--
-- This parser does NOT swallow trailing whitespace. 
--
-- Also, unlike the 'integer' parser, this parser does not admit spaces
-- between the sign and the number.
        
integer' :: MonadParser m => m Integer
integer' = int <?> "integer"

sign :: MonadParser m => m (Integer -> Integer)
sign = negate <$ char '-'
   <|> id <$ char '+'
   <|> pure id

int :: MonadParser m => m Integer
int = {-lexeme-} sign <*> nat
nat, zeroNumber :: MonadParser m => m Integer
nat = zeroNumber <|> decimal
zeroNumber = char '0' *> (hexadecimal <|> octal <|> decimal <|> return 0) <?> ""

-- | This parser parses a floating point value. Returns the value
-- of the number. The number is parsed according to the grammar rules
-- defined in the Haskell report. 
--
-- This parser does NOT swallow trailing whitespace. 

double' :: MonadParser m => m Double
double' = floating <?> "double"

floating :: MonadParser m => m Double
floating = decimal >>= fractExponent

fractExponent :: MonadParser m => Integer -> m Double
fractExponent n = (\fract expo -> (fromInteger n + fract) * expo) <$> fraction <*> option 1.0 exponent'
              <|> (fromInteger n *) <$> exponent' where
  fraction = Prelude.foldr op 0.0 <$> (char '.' *> (some digit <?> "fraction"))
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

-- | This parser parses either 'natural' or a 'float'.
-- Returns the value of the number. This parsers deals with
-- any overlap in the grammar rules for naturals and floats. The number
-- is parsed according to the grammar rules defined in the Haskell report. 
--
-- This parser does NOT swallow trailing whitespace. 

naturalOrDouble' :: MonadParser m => m (Either Integer Double)
naturalOrDouble' = natDouble <?> "number"

natDouble, zeroNumFloat, decimalFloat :: MonadParser m => m (Either Integer Double)
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

fractFloat :: MonadParser m => Integer -> m (Either Integer Double)
fractFloat n = Right <$> fractExponent n

-- | Parses a positive whole number in the decimal system. Returns the
-- value of the number. 

decimal :: MonadParser m => m Integer
decimal = number 10 digit

-- | Parses a positive whole number in the hexadecimal system. The number
-- should be prefixed with \"0x\" or \"0X\". Returns the value of the
-- number. 

hexadecimal :: MonadParser m => m Integer
hexadecimal = oneOf "xX" *> number 16 hexDigit

-- | Parses a positive whole number in the octal system. The number
-- should be prefixed with \"0o\" or \"0O\". Returns the value of the
-- number. 

octal :: MonadParser m => m Integer
octal = oneOf "oO" *> number 8 octDigit

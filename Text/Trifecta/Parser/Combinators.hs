-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Trifecta.Parser.Combinators
-- Copyright   :  (c) Edward Kmett 2011
-- License     :  BSD3
-- 
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable
-- 
-- Commonly used generic combinators
-- 
-----------------------------------------------------------------------------

module Text.Trifecta.Parser.Combinators 
  ( choice
  , option
  , optional -- from Control.Applicative, parsec optionMaybe
  , skipOptional -- parsec optional
  , between
  , skipSome -- parsec skipMany1
  , some     -- from Control.Applicative, parsec many1
  , many     -- from Control.Applicative
  , sepBy
  , sepBy1
  , sepEndBy1
  , sepEndBy
  , endBy1
  , endBy
  , count
  , chainl
  , chainr
  , chainl1
  , chainr1
  , eof
  , manyTill
  , notFollowedBy
  , lookAhead
  ) where

import Data.Traversable
import Control.Applicative
import Control.Monad
import qualified Data.ByteString as B
import Text.Trifecta.Parser.Class

-- | @choice ps@ tries to apply the parsers in the list @ps@ in order,
-- until one of them succeeds. Returns the value of the succeeding
-- parser.
choice :: Alternative m => [m a] -> m a
choice = foldr (<|>) empty

-- | @option x p@ tries to apply parser @p@. If @p@ fails without
-- consuming input, it returns the value @x@, otherwise the value
-- returned by @p@.
--
-- >  priority  = option 0 (do{ d <- digit
-- >                          ; return (digitToInt d) 
-- >                          })
option :: Alternative m => a -> m a -> m a
option x p = p <|> pure x

-- | @skipOptional p@ tries to apply parser @p@.  It will parse @p@ or nothing.
-- It only fails if @p@ fails after consuming input. It discards the result
-- of @p@. (Plays the role of parsec's optional, which conflicts with Applicative's optional)
skipOptional :: Alternative m => m a -> m ()
skipOptional p = (() <$ p) <|> pure ()

-- | @between open close p@ parses @open@, followed by @p@ and @close@.
-- Returns the value returned by @p@.
--
-- >  braces  = between (symbol "{") (symbol "}")
between :: Applicative m => m bra -> m ket -> m a -> m a
between bra ket p = bra *> p <* ket

-- | @skipSome p@ applies the parser @p@ /one/ or more times, skipping
-- its result. (aka skipMany1 in parsec)
skipSome :: MonadParser m => m a -> m ()
skipSome p = p *> skipMany p

-- | @sepBy p sep@ parses /zero/ or more occurrences of @p@, separated
-- by @sep@. Returns a list of values returned by @p@.
--
-- >  commaSep p  = p `sepBy` (symbol ",")
sepBy :: Alternative m => m a -> m sep -> m [a]
sepBy p sep = sepBy1 p sep <|> pure []

-- | @sepBy1 p sep@ parses /one/ or more occurrences of @p@, separated
-- by @sep@. Returns a list of values returned by @p@. 
sepBy1 :: Alternative m => m a -> m sep -> m [a]
sepBy1 p sep = (:) <$> p <*> many (sep *> p)

-- | @sepEndBy1 p sep@ parses /one/ or more occurrences of @p@,
-- separated and optionally ended by @sep@. Returns a list of values
-- returned by @p@. 
sepEndBy1 :: Alternative m => m a -> m sep -> m [a]
sepEndBy1 p sep = flip id <$> p <*> ((flip (:) <$> (sep *> sepEndBy p sep)) <|> pure pure)

-- | @sepEndBy p sep@ parses /zero/ or more occurrences of @p@,
-- separated and optionally ended by @sep@, ie. haskell style
-- statements. Returns a list of values returned by @p@.
--
-- >  haskellStatements  = haskellStatement `sepEndBy` semi
sepEndBy :: Alternative m => m a -> m sep -> m [a]
sepEndBy p sep = sepEndBy1 p sep <|> pure []

-- | @endBy1 p sep@ parses /one/ or more occurrences of @p@, seperated
-- and ended by @sep@. Returns a list of values returned by @p@. 
endBy1 :: Alternative m => m a -> m sep -> m [a]
endBy1 p sep = some (p <* sep)

-- | @endBy p sep@ parses /zero/ or more occurrences of @p@, seperated
-- and ended by @sep@. Returns a list of values returned by @p@.
--
-- >   cStatements  = cStatement `endBy` semi
endBy :: Alternative m => m a -> m sep -> m [a]
endBy p sep = many (p <* sep)


-- | @count n p@ parses @n@ occurrences of @p@. If @n@ is smaller or
-- equal to zero, the parser equals to @return []@. Returns a list of
-- @n@ values returned by @p@. 
count :: Applicative m => Int -> m a -> m [a]
count n p | n <= 0    = pure []
          | otherwise = sequenceA (replicate n p)


-- | @chainr p op x@ parser /zero/ or more occurrences of @p@,
-- separated by @op@ Returns a value obtained by a /right/ associative
-- application of all functions returned by @op@ to the values returned
-- by @p@. If there are no occurrences of @p@, the value @x@ is
-- returned.
chainr :: Alternative m => m a -> m (a -> a -> a) -> a -> m a
chainr p op x = chainr1 p op <|> pure x

-- | @chainl p op x@ parser /zero/ or more occurrences of @p@,
-- separated by @op@. Returns a value obtained by a /left/ associative
-- application of all functions returned by @op@ to the values returned
-- by @p@. If there are zero occurrences of @p@, the value @x@ is
-- returned.
chainl :: Alternative m => m a -> m (a -> a -> a) -> a -> m a
chainl p op x = chainl1 p op <|> pure x

-- | @chainl1 p op x@ parser /one/ or more occurrences of @p@,
-- separated by @op@ Returns a value obtained by a /left/ associative
-- application of all functions returned by @op@ to the values returned
-- by @p@. . This parser can for example be used to eliminate left
-- recursion which typically occurs in expression grammars.
--
-- >  expr    = term   `chainl1` addop
-- >  term    = factor `chainl1` mulop
-- >  factor  = parens expr <|> integer
-- >
-- >  mulop   =   do{ symbol "*"; return (*)   }
-- >          <|> do{ symbol "/"; return (div) }
-- >
-- >  addop   =   do{ symbol "+"; return (+) }
-- >          <|> do{ symbol "-"; return (-) }
chainl1 :: Alternative m => m a -> m (a -> a -> a) -> m a
chainl1 p op = scan where
  scan = flip id <$> p <*> rst
  rst = (\f y g x -> g (f x y)) <$> op <*> p <*> rst <|> pure id

-- | @chainr1 p op x@ parser /one/ or more occurrences of |p|,
-- separated by @op@ Returns a value obtained by a /right/ associative
-- application of all functions returned by @op@ to the values returned
-- by @p@.
chainr1 :: Alternative m => m a -> m (a -> a -> a) -> m a
chainr1 p op = scan where
  scan = flip id <$> p <*> rst
  rst = (flip <$> op <*> scan) <|> pure id

-- | @manyTill p end@ applies parser @p@ /zero/ or more times until
-- parser @end@ succeeds. Returns the list of values returned by @p@.
-- This parser can be used to scan comments:
--
-- >  simpleComment   = do{ string "<!--"
-- >                      ; manyTill anyChar (try (string "-->"))
-- >                      }
--
--    Note the overlapping parsers @anyChar@ and @string \"-->\"@, and
--    therefore the use of the 'try' combinator.
-- manyTill :: Alternative m => m a -> m end -> m [a]
manyTill :: (Alternative m, MonadPlus m) => m a -> m end -> m [a]
{-
manyTill p end = scan
  where 
    scan = do end; return [] 
       <|> do x <- p; xs <- scan; return (x:xs)
-}
manyTill p end = go where go = ([] <$ end) <|> ((:) <$> p <*> go)

-- * MonadParsers 

-- | This parser only succeeds at the end of the input. This is not a
-- primitive parser but it is defined using 'notFollowedBy'.
--
-- >  eof  = notFollowedBy anyChar <?> "end of input"
eof :: MonadParser m => m ()
eof = do
   l <- restOfLine 
   guard $ B.null l
 <?> "end of input"

-- | @notFollowedBy p@ only succeeds when parser @p@ fails. This parser
-- does not consume any input. This parser can be used to implement the
-- \'longest match\' rule. For example, when recognizing keywords (for
-- example @let@), we want to make sure that a keyword is not followed
-- by a legal identifier character, in which case the keyword is
-- actually an identifier (for example @lets@). We can program this
-- behaviour as follows:
--
-- >  keywordLet  = try (do{ string "let"
-- >                       ; notFollowedBy alphaNum
-- >                       })
notFollowedBy :: (MonadParser m, Show a) => m a -> m ()
notFollowedBy p = try ((try p >>= unexpected . show) <|> pure ())

-- | @lookAhead p@ parses @p@ without consuming any input.
lookAhead :: MonadParser m => m a -> m a
lookAhead p = try $ do 
  m <- mark
  p <* release m


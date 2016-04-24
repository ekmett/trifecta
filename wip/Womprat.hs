{-# LANGUAGE ImplicitPrelude #-}
{-# LANGUAGE RecordWildCards, ViewPatterns, FlexibleInstances, MultiParamTypeClasses #-}

import Text.Parsec
import qualified Text.Parsec.Token as T
import Text.Parsec.Token (GenLanguageDef(..), GenTokenParser(TokenParser))
import Text.Parsec.Pos (initialPos, updatePosChar)
import Data.Functor.Identity (Identity(..))
import Control.Applicative hiding ((<|>))
import Control.Monad.Fix (fix)

(</>) :: Monad m => ParsecT s u m a -> ParsecT s u m a -> ParsecT s u m a
p </> q = try p <|> q
infixl 3 </>

type Result d a = Consumed (Reply d () a)

womp :: d -> SourcePos -> ParsecT d () Identity a -> Result d a
womp d pos p = fmap runIdentity . runIdentity $ runParsecT p (State d pos ())

rat :: Monad m => (d -> Result d a) -> ParsecT d u m a
rat f   = mkPT $ \s0 -> return $ return . patch s0 <$> f (stateInput s0) where
  patch (State _ _ u) (Ok a (State s p _) err) = Ok a (State s p u) err
  patch _             (Error e)                = Error e

myLanguageDef :: Monad m => T.GenLanguageDef D u m
myLanguageDef = T.LanguageDef
  { commentStart    = "{-"
  , commentEnd      = "-}"
  , commentLine     = "--"
  , nestedComments  = True
  , identStart      = letter <|> char '_'
  , identLetter     = alphaNum <|> oneOf "_'"
  , opStart         = opLetter myLanguageDef
  , opLetter        = oneOf ":!#$%&*+./<=>?@\\^|-~"
  , reservedOpNames = []
  , reservedNames   = []
  , caseSensitive   = True
  }

TokenParser {..} = T.makeTokenParser myLanguageDef

-- * Building a packrat parser with parsec

-- I used to bullseye womp rats in my T-16 back home.

data D = D
  { _add        :: Result D Integer
  , _mult       :: Result D Integer
  , _primary    :: Result D Integer
  , _dec        :: Result D Integer
  , _uncons     :: Maybe (Char, D)
  }

instance Monad m => Stream D m Char where
  uncons = return . _uncons

add, mult, primary, dec :: Parsec D u Integer
add     = rat _add
mult    = rat _mult
primary = rat _primary
dec     = rat _dec

prep :: SourceName -> String -> D
prep n = go (initialPos n) where
  go p s = fix $ \d -> let
    (womp d p -> _add) = (+) <$> mult <* reservedOp "+" <*> add </> mult <?> "summand"
    (womp d p -> _mult) = (*) <$> primary <* reservedOp "*" <*> mult </> primary <?> "factor"
    (womp d p -> _primary) = parens add </> dec <?> "number"
    (womp d p -> _dec) = natural
    _uncons = case s of
      (x:xs) -> Just (x, go (updatePosChar p x) xs)
      []     -> Nothing
    in D { .. }

runD :: Parsec D u a -> u -> SourceName -> String -> Either ParseError a
runD p u fn s = runParser p u fn (prep fn s)

eval :: String -> Integer
eval s = either (error . show) id $ runD (whiteSpace *> add <* eof) () "-" s

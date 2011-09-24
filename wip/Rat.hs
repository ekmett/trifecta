{-# LANGUAGE RecordWildCards, ViewPatterns, DeriveFunctor, FlexibleInstances #-}
import Control.Applicative
import Control.Monad (MonadPlus(..), guard)
import Control.Monad.Fix (fix)
import Data.Char (isDigit, digitToInt, isSpace)

data Result d a
  = Pure a           -- we didn't consume anything and can backtrack
  | Commit d a       -- we consumed input
  | Fail String Bool -- we failed with a flag indicating if we have consumed input
  deriving Functor

newtype Rat d a = Rat { runRat :: d -> Result d a }
  deriving Functor

instance Applicative (Rat d) where
  pure a = Rat $ \ _ -> Pure a
  Rat mf <*> Rat ma = Rat $ \ d -> case mf d of
    Pure f      -> fmap f (ma d)
    Fail s c    -> Fail s c
    Commit d' f -> case ma d' of
      Pure a       -> Commit d' (f a)
      Fail s _     -> Fail s True
      Commit d'' a -> Commit d'' (f a)

instance Alternative (Rat d) where
  Rat ma <|> Rat mb = Rat $ \ d -> case ma d of
    Fail _ False -> mb d
    x            -> x
  empty = Rat $ \ _ -> Fail "empty" False

instance Monad (Rat d) where
  return a = Rat $ \_ -> Pure a
  Rat m >>= k = Rat $ \d -> case m d of
    Pure a -> runRat (k a) d
    Commit d' a -> case runRat (k a) d' of
      Pure b -> Commit d' b
      Fail s _ -> Fail s True
      commit -> commit
    Fail s c -> Fail s c
  fail s = Rat $ \ _ -> Fail s False

instance MonadPlus (Rat d) where
  mplus = (<|>)
  mzero = empty

try :: Rat d a -> Rat d a
try (Rat m) = Rat $ \d -> case m d of
  Fail s _ -> Fail s False
  x        -> x

(</>) :: Rat d a -> Rat d a -> Rat d a
p </> q = try p <|> q
infixl 3 </>

class Stream d where
  anyChar :: Rat d Char

instance Stream [Char] where
  anyChar = Rat $ \s -> case s of
    (x:xs) -> Commit xs x
    [] -> Fail "EOF" False

whiteSpace :: Stream d => Rat d ()
whiteSpace = () <$ many (satisfy isSpace)

phrase :: Stream d => Rat d a -> Rat d a
phrase m = whiteSpace *> m <* eof

notFollowedBy :: Rat d a -> Rat d ()
notFollowedBy (Rat m) = Rat $ \d -> case m d of
  Fail{} -> Pure ()
  _      -> Fail "unexpected" False

eof :: Stream d => Rat d ()
eof = notFollowedBy $ anyChar

satisfy :: Stream d => (Char -> Bool) -> Rat d Char
satisfy p = try $ do
  x <- anyChar
  x <$ guard (p x)

char :: Stream d => Char -> Rat d Char
char c = satisfy (c ==)

lexeme :: Stream d => Rat d a -> Rat d a
lexeme m = m <* whiteSpace

symbol :: Stream d => Char -> Rat d Char
symbol c = lexeme (char c)

digit :: Stream d => Rat d Int
digit = digitToInt <$> satisfy isDigit

data D = D
  { _add        :: Result D Int
  , _mult       :: Result D Int
  , _primary    :: Result D Int
  , _decimal    :: Result D Int
  , anyCharD    :: Result D Char
  }

-- makeRat ''D should output:
add, mult, primary, decimal :: Rat D Int
add     = Rat _add
mult    = Rat _mult
primary = Rat _primary
decimal = Rat _decimal

dv :: d -> (d -> b) -> b
dv d f = f d

instance Stream D where
  anyChar = Rat anyCharD

parse :: String -> D
parse s = fix $ \d -> let
  Rat (dv d -> _add) = (+) <$> mult <* symbol '+' <*> add </> mult
  Rat (dv d -> _mult) = (*) <$> primary <* symbol '*' <*> mult </> primary
  Rat (dv d -> _primary) = symbol '(' *> add <* symbol ')' </> decimal
  Rat (dv d -> _decimal) = foldl (\b a -> b * 10 + a) 0 <$> lexeme (some digit)
  anyCharD = case s of
    (x:xs) -> Commit (parse xs) x
    []     -> Fail "EOF" False
  in D { .. }

eval :: String -> Int
eval s = case runRat (whiteSpace *> add <* eof) (parse s) of
  Pure a -> a
  Commit _ a -> a
  _ -> error "Parse error"

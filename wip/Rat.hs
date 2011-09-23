import Control.Applicative
import Control.Monad (MonadPlus(..), guard)
import Data.Char (isDigit, digitToInt, isSpace)
import Data.Foldable
import Data.Traversable
import Data.Semigroup
import Text.Trifecta.Rope.Delta

-- a parsec-like packrat parser

data Result d a
  = Pure a           -- we didn't consume anything and can backtrack
  | Commit d a       -- we consumed input
  | Fail String Bool -- we failed with a flag indicating if we have consumed input

instance Functor (Result d) where
  fmap f (Pure a)     = Pure (f a)
  fmap f (Commit d a) = Commit d (f a)
  fmap _ (Fail s c)   = Fail s c

instance Foldable (Result d) where
  foldMap f (Pure a) = f a
  foldMap f (Commit _ a) = f a
  foldMap _ _ = mempty

instance Traversable (Result d) where
  traverse f (Pure a)     = Pure <$> f a
  traverse f (Commit d a) = Commit d <$> f a
  traverse f (Fail s b)   = pure $ Fail s b

newtype Rat d a = Rat { runRat :: d -> Result d a }

instance Functor (Rat d) where
  fmap f (Rat m) = Rat $ \ d -> fmap f (m d)

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
      Fail s c -> Fail s True
      commit -> commit
    Fail s c -> Fail s c
  fail s = Rat $ \ d -> Fail s False

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

commit :: Rat d a -> Rat d a
commit (Rat m) = Rat $ \d -> case m d of
  Pure a   -> Commit d a
  Fail s _ -> Fail s True
  c        -> c

notFollowedBy :: Rat d a -> Rat d ()
notFollowedBy (Rat m) = Rat $ \d -> case m d of
  Fail _ True  -> Commit d ()
  Fail _ False -> Pure ()
  Commit _ _   -> Fail "unexpected" True
  Pure _       -> Fail "unexpected" False

lookAhead :: Rat d a -> Rat d a
lookAhead (Rat m) = Rat $ \d -> case m d of
  Commit _ a -> Pure a
  x          -> x

mark :: Rat d d
mark = Rat $ \d -> Pure d

release :: d -> Rat d ()
release d = Rat $ \_ -> Commit d ()

class Memo d where
  memo    :: Delta -> String -> Result d Char -> d

  anyChar :: Rat d Char

  rest :: Rat d String
  rest = many anyChar

  whiteSpace :: Rat d ()
  whiteSpace = () <$ many (satisfy isSpace) <?> "space"

phrase :: Memo d => Rat d a -> Rat d a
phrase m = whiteSpace *> m <* eof

eof :: Memo d => Rat d ()
eof = notFollowedBy anyChar

parse :: Memo d => String -> d
parse = go mempty where
 go d s = memo d s $ case s of
  (x:xs) -> Commit (go (d <> delta x) xs) x
  []     -> Fail "EOF" False

satisfy :: Memo d => (Char -> Bool) -> Rat d Char
satisfy p = try $ do
  x <- anyChar
  x <$ guard (p x)

char :: Memo d => Char -> Rat d Char
char c = satisfy (c ==) <?> show c

lexeme :: Memo d => Rat d a -> Rat d a
lexeme m = m <* whiteSpace

symbol :: Memo d => Char -> Rat d Char
symbol c = lexeme (char c)

digit :: Memo d => Rat d Int
digit = digitToInt <$> satisfy isDigit

infix 0 <?>
(<?>) :: Rat d a -> String -> Rat d a
p <?> _ = p

data D = D
  { _add        :: Result D Int
  , _mult       :: Result D Int
  , _primary    :: Result D Int
  , _decimal    :: Result D Int
  , deltaD      :: Delta
  , restD       :: String
  , anyCharD    :: Result D Char
  }

-- makeRat ''D should output:
add, mult, primary, decimal :: Rat D Int
add     = Rat _add
mult    = Rat _mult
primary = Rat _primary
decimal = Rat _decimal

instance Memo D where
  anyChar = Rat anyCharD
  rest = Rat $ \d -> Pure $ restD d
  memo p s c = d where
    d = D (add_ d) (mult_ d) (primary_ d) (decimal_ d) p s c
    Rat add_ = (+) <$> mult <* symbol '+' <*> add
           </> mult <?> "summand"
    Rat mult_ = (*) <$> primary <* symbol '*' <*> mult
            </> primary <?> "factor"
    Rat primary_ = symbol '(' *> add <* symbol ')'
               </> decimal <?> "number"
    Rat decimal_ = foldl' (\b a -> b * 10 + a) 0 <$> lexeme (some digit) <?> "digit"

eval :: String -> Int
eval s = case runRat (whiteSpace *> add <* eof) (parse s) of
  Pure a -> a
  Commit _ a -> a
  _ -> error "Parse error"

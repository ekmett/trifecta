-- | This module provides a short introduction to get users started using
-- Trifecta. The key takeaway message is that it’s not harder, or even much
-- different, from using other parser libraries, so for users familiar with one
-- of the many Parsecs should feel right at home.
--
-- __The source of this file is written in a literate style__, and can be read
-- top-to-bottom.
module Text.Trifecta.Tutorial where

import Control.Applicative
import Text.Trifecta

-- | First, we import Trifecta itself. It only the core parser definitions and
-- instances. Since Trifecta on its own is just the parser and a handful of
-- instances; the bulk of the utility functions is actually from a separate
-- package, /parsers/, that provides the usual parsing functions like
-- 'manyTill', 'between', and so on. The idea behind the /parsers/ package is
-- that most parser libraries define the same generic functions, so they were
-- put into their own package to be shared. Trifecta reexports these
-- definitions, but it’s useful to keep in mind that the documentation of
-- certain functions might not be directly in the /trifecta/ package.
importDocumentation :: docDummy
importDocumentation = error "Auxiliary definition to write Haddock documetation for :-)"

-- | In order to keep things minimal, we define a very simple language for
-- arithmetic expressions.
data Expr
    = Add Expr Expr -- ^ expr + expr
    | Lit Integer   -- ^ 1, 2, -345, …
    deriving (Show)

-- | The parser is straightforward: there are literal integers, and
-- parenthesized additions. We require parentheses in order to keep the example
-- super simple as to not worry about operator precedence.
--
-- It is useful to use /tokenizing/ functions to write parsers. Roughly
-- speaking, these automatically skip trailing whitespace on their own, so that
-- the parser isn’t cluttered with 'skipWhitespace' calls. 'symbolic' for
-- example parses a 'Char' and then skips trailing whitespace; there is also the
-- more primitive 'char' function that just parses its argument and nothing
-- else.
parseExpr :: Parser Expr
parseExpr = parseAdd <|> parseLit
  where
    parseAdd = parens $ do
        x <- parseExpr
        _ <- symbolic '+'
        y <- parseExpr
        pure (Add x y)
    parseLit = Lit <$> integer

-- | We can now use our parser to convert a 'String' to an 'Expr',
--
-- @
-- parseString parseExpr mempty "(1 + (2 + 3))"
-- @
--
-- > Success (Add (Lit 1) (Add (Lit 2) (Lit 3)))
--
-- When we provide ill-formed input, we get a nice error message with an arrow
-- to the location where the error occurred:
--
-- @
-- parseString parseExpr mempty "(1 + 2 + 3))"
-- @
--
-- > (interactive):1:8: error: expected: ")"
-- > 1 | (1 + 2 + 3))<EOF>
-- >   |        ^
examples :: docDummy
examples = error "Haddock dummy for documentation"

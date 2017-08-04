-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2011-2015 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- >>> import Text.Parser.Char
-- >>> import Text.Parser.Combinators
-- >>> import Text.Parser.Token
-- >>> import Control.Applicative
-- >>> data Expr = Add Expr Expr | Mul Expr Expr | Lit Integer deriving (Show)
-- >>> :{
-- >>> let parens = between (symbolic '(') (symbolic ')')
-- >>>     parseAdd = parens (do { x <- parseExpr; symbolic '+'; y <- parseExpr; pure (Add x y) })
-- >>>     parseMul = parens (do { x <- parseExpr; symbolic '*'; y <- parseExpr; pure (Mul x y) })
-- >>>     parseLit = Lit <$> integer
-- >>>     parseExpr = parseAdd <|> parseMul <|> parseLit
-- >>> in parseString parseExpr mempty "(1 + 2)"
-- >>> :}
-- Put expected result here
----------------------------------------------------------------------------
module Text.Trifecta
  ( module Text.Trifecta.Rendering
  , module Text.Trifecta.Highlight
  , module Text.Trifecta.Parser
  , module Text.Trifecta.Combinators
  , module Text.Trifecta.Result
  , module Text.Trifecta.Rope
  , module Text.Parser.Combinators
  , module Text.Parser.Char
  , module Text.Parser.Token
  ) where

import Text.Trifecta.Rendering
import Text.Trifecta.Highlight
import Text.Trifecta.Parser
import Text.Trifecta.Combinators
import Text.Trifecta.Result
import Text.Trifecta.Rope
import Text.Parser.Combinators
import Text.Parser.Char
import Text.Parser.Token

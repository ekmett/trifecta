-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Trifecta.Literate.Combinators
-- Copyright   :  (C) 2011 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
----------------------------------------------------------------------------
module Text.Trifecta.Literate.Combinators
  ( someLiterateSpace
  , getLiterate
  , putLiterate
  ) where

import Data.Char (isSpace)
import Control.Applicative
import Control.Monad
import Text.Trifecta.Rope.Delta
import qualified Text.Trifecta.Highlight.Prim as Highlight
import Text.Trifecta.Literate.Prim
import Text.Trifecta.Literate.Class
import Text.Trifecta.Parser.Char
import Text.Trifecta.Parser.Class
import Text.Trifecta.Parser.Combinators

getLiterate :: MonadLiterate m => m LiterateState
getLiterate = literateState $ \s -> (s, s)

putLiterate :: MonadLiterate m => LiterateState -> m ()
putLiterate s = literateState $ \_ -> ((), s)

skipLine :: MonadParser m => m ()
skipLine = do
  r <- restOfLine
  skipping (delta r)

someLiterateSpace :: MonadLiterate m => m ()
someLiterateSpace = do
  s <- getLiterate
  case s of
    IlliterateStart -> skipSome $ satisfy isSpace
    LiterateStart -> position >>= \m -> track m <|> begin m <|> blank m <|> other m
    LiterateCode  -> do
      skipSome $ satisfy isSpace
      option () $ position >>= \m -> when (column m == 0) $ do
        highlight Highlight.LiterateSyntax (string "\\end{code}") *> skipLine
        begin m <|> blank m <|> other m
    LiterateTrack -> skipSome horizontalSpace >> skipOptional bird
                 <|> bird
  where
    bird = newline *> position >>= \m -> track m <|> blank m

-- parse space excluding newlines
horizontalSpace :: MonadLiterate m => m Char
horizontalSpace = satisfy $ \s -> s /= '\n' && isSpace s

track, begin, blank, other :: MonadLiterate m => Delta -> m ()
track s = do
  e <- position
  try $ highlight Highlight.LiterateSyntax (char '>')
     *> highlightInterval Highlight.LiterateComment s e
     *> skipSome horizontalSpace
  tracks <|> putLiterate LiterateTrack where
    tracks = do
      s' <- newline *> position
      track s' <|> blank s'

begin s = do
  try $ highlight Highlight.LiterateSyntax (string "\begin{code}") *> skipLine
  position >>= highlightInterval Highlight.LiterateComment s
  putLiterate LiterateCode
  whiteSpace

other s = do
  notChar '>' *> skipLine
  begin s <|> blank s <|> other s

blank s = do
  k <- try $ do
    skipMany horizontalSpace
    True <$ newline <|> False <$ eof
  when k $ track s <|> begin s <|> blank s <|> other s

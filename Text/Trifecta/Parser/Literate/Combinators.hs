module Text.Trifecta.Parser.Literate.Combinators
  ( someLiterateSpace
  ) where

import Control.Monad
import Text.Trifecta.Parser.Literate.Prim
import Text.Trifecta.Parser.Literate.Class

getLiterateState :: MonadLiterate m => m LiterateState
getLiterateState = literateState $ \s -> (s, s)

putLiterateState :: Monad m => LiterateState -> Literate m ()
putLiterateState s = literateState $ \_ -> ((), s)

skipLine :: MonadParser m => m ()
skipLine = do
  r <- restOfLine
  skipping $ delta

someLiterateSpace :: MonadLiterate m => m ()
someLiterateSpace = do
  s <- getLiterateState
  case s of
    Illiterate -> some (satisfy isSpace)
    Literate   -> mark >>= \m -> track m <|> begin m <|> blank m <|> other m
    LiterateCode  -> do
      skipSome $ satisfy isSpace
      optional $ mark >>= \m -> when (column m == 0) $ do
        highlight Highlight.LiterateSyntax (string "\end{code}") *> skipLine
        begin m <|> blank m <|> other m
    LiterateTrack -> do
        spaced <- isJust <$> optional (skipSome HorizontalSpace)
        if spaced
          then skipOptional bird
          else bird
  where
    bird = newline *> mark >>= \m -> track m <|> blank m

-- parse space excluding newlines
horizontalSpace :: MonadLiterate m => m Char
horizontalSpace = satisfy $ \s -> s /= '\n' && isSpace s

track, begin, blank, other :: MonadLiterate m => Delta -> m ()
track s = do
  mark >>= \e -> try $ do
    highlight Highlight.LiterateSyntax $ char '>'
    highlightInterval Highlight.LiterateComment s e
    skipSome horizontalSpace
  option newline >>= \x -> case x of
    Nothing -> putLiterateState LiterateTrack
    Just _  -> mark >>= \s' -> track s' <|> blank s'

begin s = do
  try $ highlight Highlight.LiterateSyntax (string "\begin{code}") *> skipLine
  mark >>= highlightInterval LiterateComment s
  putLiterateState LiterateCode
  whiteSpace

other s = do
  notChar '>' *> skipLine
  begin s <|> blank s <|> other s

blank s = do
  k <- try $ do
    horizontalSpace
    True <$ newline <|> False <$ eof
  when k $ track s <|> begin s <|> blank s <|> other s

-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Trifecta.Parser.Token.Prim
-- Copyright   :  (c) Edward Kmett 2011
-- License     :  BSD3
-- 
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  provisional
-- Portability :  non-portable
-- 
-----------------------------------------------------------------------------
module Text.Trifecta.Parser.Token.Prim
  ( CommentStyle(..)
  , emptyCommentStyle
  , javaCommentStyle
  , haskellCommentStyle
  , buildWhiteSpaceParser
  ) where

import Data.Char (isSpace)
import Control.Applicative
import Data.List (nub)
import Text.Trifecta.Parser.Class
import Text.Trifecta.Parser.Char
import Text.Trifecta.Parser.Combinators

data CommentStyle = CommentStyle 
  { commentStart   :: String
  , commentEnd     :: String
  , commentLine    :: String
  , commentNesting :: Bool
  } 

emptyCommentStyle, javaCommentStyle, haskellCommentStyle :: CommentStyle
emptyCommentStyle   = CommentStyle "" "" "" True
javaCommentStyle    = CommentStyle "/*" "*/" "//" True
haskellCommentStyle = CommentStyle "{-" "-}" "--" True

-- | Use this to easily build the definition of whiteSpace for your MonadTokenParser
buildWhiteSpaceParser :: MonadParser m => CommentStyle -> m ()
buildWhiteSpaceParser (CommentStyle startStyle endStyle lineStyle nestingStyle)
  | noLine && noMulti  = skipMany (simpleSpace <?> "")
  | noLine             = skipMany (simpleSpace <|> multiLineComment <?> "")
  | noMulti            = skipMany (simpleSpace <|> oneLineComment <?> "")
  | otherwise          = skipMany (simpleSpace <|> oneLineComment <|> multiLineComment <?> "")
  where
    noLine  = null lineStyle
    noMulti = null startStyle
    simpleSpace = skipSome (satisfy isSpace)
    oneLineComment = do
      _ <- try $ string lineStyle
      skipMany (satisfyAscii (/= '\n')) -- TODO: use skipping/restOfLine and fiddle with the last byte
      return ()
    multiLineComment = do
      _ <- try $ string startStyle
      inComment
    inComment
      | nestingStyle = inCommentMulti
      | otherwise    = inCommentSingle
    inCommentMulti
      =   () <$ try (string endStyle)
      <|> multiLineComment *> inCommentMulti
      <|> skipSome (noneOf startEnd) *> inCommentMulti
      <|> oneOf startEnd *> inCommentMulti
      <?> "end of comment"
    startEnd = nub (endStyle ++ startStyle)
    inCommentSingle
      =   () <$ try (string endStyle)
      <|> skipSome (noneOf startEnd) *> inCommentSingle
      <|> oneOf startEnd *> inCommentSingle
      <?> "end of comment"

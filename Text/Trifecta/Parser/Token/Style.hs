-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Trifecta.Parser.Token.Style
-- Copyright   :  (c) Edward Kmett 2011
-- License     :  BSD3
--
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  provisional
-- Portability :  non-portable
--
-----------------------------------------------------------------------------
module Text.Trifecta.Parser.Token.Style
  ( CommentStyle(..)
  , emptyCommentStyle
  , javaCommentStyle
  , haskellCommentStyle
  , buildSomeSpaceParser
  ) where

import Control.Applicative
import Data.List (nub)
import qualified Data.ByteString.Char8 as B
import Text.Trifecta.Parser.Class
import Text.Trifecta.Parser.Char
import Text.Trifecta.Parser.Combinators
import Text.Trifecta.Rope.Delta
import Text.Trifecta.Highlight.Prim

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

-- | Use this to easily build the definition of whiteSpace for your MonadParser
--   given a comment style and an underlying someWhiteSpace parser
buildSomeSpaceParser :: MonadParser m => m () -> CommentStyle -> m ()
buildSomeSpaceParser simpleSpace (CommentStyle startStyle endStyle lineStyle nestingStyle)
  | noLine && noMulti  = skipSome (simpleSpace <?> "")
  | noLine             = skipSome (simpleSpace <|> multiLineComment <?> "")
  | noMulti            = skipSome (simpleSpace <|> oneLineComment <?> "")
  | otherwise          = skipSome (simpleSpace <|> oneLineComment <|> multiLineComment <?> "")
  where
    noLine  = null lineStyle
    noMulti = null startStyle
    oneLineComment = highlight Comment $ do
      _ <- try $ string lineStyle
      r <- restOfLine
      let b = B.length r
      skipping $ if b /= 0 && B.last r == '\n' then Lines 1 0 (fromIntegral b) 0 else delta r
    multiLineComment = highlight Comment $ do
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

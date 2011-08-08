{-# LANGUAGE MultiParamTypeClasses, BangPatterns #-}
module Text.Trifecta.Parser
  ( P
  , line
  , slice
  , sliced
  , careted
  , spanned
  , fixit
  ) where

import Control.Applicative
import Control.Monad.Trans.Class
import Data.Semigroup
import Data.Interned
import Data.Foldable (toList)
import Data.FingerTree as FingerTree
import Data.ByteString as Strict
import Data.ByteString.Lazy as Lazy
import Text.Trifecta.Hunk
import Text.Trifecta.Bytes
import Text.Trifecta.Rope as Rope
import Text.Trifecta.Delta
import Text.Trifecta.Strand
import Text.Trifecta.Caret
import Text.Trifecta.Span
import Text.Trifecta.Fixit
import Text.Trifecta.It
import Text.Parsec.Prim hiding ((<|>))

type P u = ParsecT Delta u It

-- grab the contents of the line that contains delta
line :: Delta -> P u Strict.ByteString
line d = lift $ Strict.concat 
              . Lazy.toChunks 
              . Lazy.takeWhile (/= 10)
              . snd <$> peekIt (rewind d)

slice :: Delta -> Delta -> P u Strict.ByteString
slice !i !j = lift (Cont loop)
  where
    bi = bytes i
    bj = bytes j
    loop t eof
      | bj <= bytes t || eof = Done t eof $ go $ strands t
      | otherwise = Cont $ \t' eof' -> loop (t <> t') eof'
    go !t
      | required <= Strict.length first = Strict.take required first
      | otherwise = Strict.concat 
                  $ Lazy.toChunks 
                  $ Lazy.take (fromIntegral required) 
                  $ Lazy.fromChunks 
                  $ first : Prelude.foldr iter [] (toList r')
      where 
        iter (HunkStrand h) b = unintern h : b
        iter (PathStrand _) b = b
        (l,r) = FingerTree.split (\m -> bytes m > bi) t
        HunkStrand (Hunk _ _ a) :< r' = FingerTree.viewl r
        first = Strict.drop (bi - bytes l) a
        required = bj - bi

sliced :: P u a -> P u Strict.ByteString
sliced pa = do
  mark <- getInput
  _ <- pa
  release <- getInput
  slice mark release

spanned :: P u a -> P u (Spanned a)
spanned p = do
  m <- getInput
  l <- line m
  a <- p
  r <- getInput
  return $ a :~ Span m r l

careted :: P u a -> P u (Careted a)
careted p = do
  m <- getInput
  l <- line m
  a <- p 
  return $ a :^ Caret m l

fixit :: P u Strict.ByteString -> P u Fixit
fixit p = (\(rep :~ s) -> Fixit s rep) <$> spanned p

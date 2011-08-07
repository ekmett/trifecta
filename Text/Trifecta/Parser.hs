{-# LANGUAGE MultiParamTypeClasses, BangPatterns #-}
module Text.Trifecta.Parser
  ( P
  , line
  , slice
  , sliced
  , careted
  , covered
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Data.Semigroup
import Data.Monoid
import Data.Word
import Data.Interned
import Data.Foldable (toList)
import Data.FingerTree as FingerTree
import Data.ByteString as Strict
import Data.ByteString.Lazy as Lazy
import Data.Functor.Bind
import Data.Functor.Plus
import Text.Trifecta.Hunk
import Text.Trifecta.Rope as Rope
import Text.Trifecta.Delta
import Text.Trifecta.It
import Text.Parsec.Prim hiding ((<|>))

type P u = ParsecT Delta u It

-- grab the contents of the line that contains delta
line :: Delta -> P u Strict.ByteString
line d = lift $ Strict.concat 
              . Lazy.toChunks 
              . Lazy.takeWhile (/='\n') 
              . fst <$> peekIt (rewind d)

slice :: Delta -> Delta -> P u Strict.ByteString
slice !i !j = lift $ Cont go 
  where
    go :: Rope -> Bool -> It Strict.ByteString
    go t eof
      | j <= bytes t || eof = Done t eof (chop (strands h))
      | otherwise = Cont $ \t' eof' -> go (t <> t') eof'
    chop !t
      | req <= rmn = Strict.take req first -- yay! sharing
      | otherwise = Strict.concat 
                  $ Lazy.toChunks 
                  $ Lazy.take (fromIntegral req) 
                  $ Lazy.fromChunks 
                  $ first : Prelude.foldr iter [] (toList r')
      where 
        bi = bytes i
        bj = bytes j
        iter (HunkStrand h) b = unintern h : b
        iter (PathStrand _) b = b
        (l,r) = FingerTree.split (\m -> bytes m > bi) t
        HunkStrand (Hunk _ _ a) :< r' = FingerTree.viewl r
        first = Strict.drop (bi - cursorBytes (measure l)) a
        req = bj - bi
        rmn = Strict.length first

sliced :: P u a -> P u Strict.ByteString
sliced pa = do
  mark <- getInput
  _ <- pa
  release <- getInput
  slice mark release


covered :: P u a -> P u a
covered pa = do
  mark    <- getInput
  line    <- lift (lineIt mark)
  a       <- pa
  release <- getInput
  return $ a :~ Cover (Caret mark line) release

careted :: P a -> P u (Careted a)
careted p = do
  mark <- getInput
  line <- lift (lineIt mark)
  a    <- pa 
  return $ a :^ Caret mark line

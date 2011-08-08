{-# LANGUAGE MultiParamTypeClasses, BangPatterns #-}
module Text.Trifecta.Slice
  ( slice
  , sliced
  ) where

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
import Text.Trifecta.It
import Text.Parsec.Prim hiding ((<|>))

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

sliced :: (a -> Strict.ByteString -> r) -> P u a -> P u r
sliced f pa = do
  mark <- getInput
  a <- pa
  release <- getInput
  bs <- slice mark release
  return $ f a bs

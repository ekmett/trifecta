{-# LANGUAGE FlexibleContexts #-}
module Text.Trifecta.Supply 
  ( Supply(..)
  , EOF(EOF)
  , supplyEOF
  -- * type restricted versions of supply
  , supplyDefault
  , supplyStrand
  , supplyHunk
  , supplyPath
  , supplyByteString
  ) where

import Control.Parallel.Strategies hiding (Done)
import Data.Monoid
import Data.Semigroup.Reducer
import Data.Foldable
import Data.Word
import Data.Semigroup.Foldable
import Data.List.NonEmpty
import Data.FingerTree as FingerTree
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Lazy as Lazy
import Data.ByteString.UTF8 as UTF8
import Text.Trifecta.Path
import Text.Trifecta.Rope
import Text.Trifecta.Hunk
import Text.Trifecta.Strand
import Text.Trifecta.It

-- enumerators for our iteratee

class Supply t where
  supply :: t -> It a -> It a 
  supplyList :: [t] -> It a -> It a 
  supplyList = Prelude.foldr (\t b -> b . supply t) id

data EOF = EOF

instance Supply EOF where
  supply _ = supplyEOF

supplyEOF :: It a -> It a
supplyEOF (Cont k)     = k mempty True
supplyEOF (Done r _ a) = Done r True a
supplyEOF (Fail r _ a) = Fail r True a

supplyDefault :: Reducer t Rope => t -> It a -> It a
supplyDefault new (Done old _ a) = Done (snoc old new) False a
supplyDefault new (Fail old _ a) = Fail (snoc old new) False a
supplyDefault new (Cont k)       = k (unit new) False

supplyStrand :: Strand -> It a -> It a 
supplyStrand = supplyDefault

supplyHunk :: Hunk -> It a -> It a
supplyHunk = supplyDefault

supplyPath :: Path -> It a -> It a
supplyPath = supplyDefault

supplyByteString :: ByteString -> It a -> It a 
supplyByteString = supplyDefault

-- DO NOT WANT
instance Supply Word8 where
  supply = supplyByteString . Strict.singleton
  supplyList = supplyByteString . Strict.pack

instance Supply Char where
  supply c = supplyByteString (UTF8.fromString [c])
  supplyList = supplyByteString . UTF8.fromString

instance Supply a => Supply [a] where
  supply = supplyList

instance Supply Strict.ByteString where
  supply = supplyByteString
  supplyList = supplyList . Prelude.map (HunkStrand . hunk)

instance Supply Lazy.ByteString where
  supply = supplyList . Lazy.toChunks 

instance Supply Hunk where
  supply = supplyHunk
  supplyList = supplyList . Prelude.map HunkStrand

instance Supply Path where
  supply = supplyPath
  supplyList [] = id
  supplyList (x:xs) = supplyPath $ fold1 (x :| xs)

instance Supply Strand where
  supply = supplyStrand
  supplyList xs = supply (rope (FingerTree.fromList xs')) where
     !xs' = withStrategy (evalList rseq) xs

instance Supply Rope where
  supply = supplyDefault
  supplyList = supplyDefault . fold

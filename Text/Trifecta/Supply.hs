module Text.Trifecta.Supply 
  ( Supply(..)
  , EOF(..)
  , supplyEOF
  , supplyRope
  , supplyStrand
  , supplyHunk
  , supplyPath
  , supplyByteString
  ) where

import Text.Trifecta.Path
import Text.Trifecta.Rope
import Text.Trifecta.Hunk
import Text.Trifecta.It
import Data.Interned
import Data.Monoid
import Data.Semigroup
import Data.Foldable
import Data.Word
import Data.Semigroup.Foldable
import Data.List.NonEmpty
import Data.FingerTree as FingerTree
import Data.ByteString as Strict
import Data.ByteString.UTF8 as UTF8
import Control.Parallel.Strategies hiding (Done)

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

supplyRope :: Rope -> It a -> It a
supplyRope new (Done old _ a) = Done (old <> new) False a
supplyRope new (Fail old _ a) = Fail (old <> new) False a
supplyRope new (Cont k) = k new False

supplyStrand :: Strand -> It a -> It a 
supplyStrand = supplyRope . intern . FingerTree.singleton

supplyHunk :: Hunk -> It a -> It a
supplyHunk = supplyStrand . HunkStrand

supplyPath :: Path -> It a -> It a
supplyPath = supplyStrand . PathStrand

supplyByteString :: ByteString -> It a -> It a 
supplyByteString bs = supplyHunk (intern bs)

instance Supply Word8 where
  supply = supplyByteString . Strict.singleton
  supplyList = supplyByteString . Strict.pack

instance Supply Char where
  supply c | fromEnum c <= 0x7f = supplyByteString $ Strict.singleton $ toEnum $ fromEnum c
           | otherwise = error "TODO"
        -- TODO: more
  supplyList = supplyByteString . UTF8.fromString

instance Supply a => Supply [a] where
  supply = supplyList

instance Supply ByteString where
  supply = supplyByteString
  supplyList = supplyList . Prelude.map (HunkStrand . intern)

instance Supply Hunk where
  supply = supplyHunk
  supplyList = supplyList . Prelude.map HunkStrand

instance Supply Path where
  supply = supplyPath
  supplyList [] = id
  supplyList (x:xs) = supplyPath $ fold1 (x:|xs)

instance Supply Strand where
  supply = supplyStrand
  supplyList xs = supplyRope (intern (FingerTree.fromList xs')) where
     !xs' = withStrategy (evalList rseq) xs

instance Supply Rope where
  supply = supplyRope
  supplyList = supplyRope . fold

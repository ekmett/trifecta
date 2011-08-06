{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FlexibleInstances #-}
module Text.Trifecta.Strand
  (
  , Sid
  , HasSid(..)
  , Sids
  , HasSids(..)
  , compatible
  ) where

import Data.Interned
import Data.Hashable
import Data.Semigroup.Reducer.With
import Data.ByteString as Strict
import Data.FingerTree as FingerTree
import Text.Trifecta.Hunk
import Text.Trifecta.Path
import Text.Trifecta.Summary
import Text.Trifecta.Delta
import Text.PrettyPrint.Leijen.Extras

-- *** Strand Id

type Sid = Int

class HasSids t => HasSid t where
  sid :: t -> Int

type Sids = FingerTree Tally (WithReducer Tally Int)

class HasSids t where
  sids :: t -> Sids

compatible :: HasSids t => t -> t -> Bool
compatible a b = case compare nx ny of
  LT | y' <- trim nx y -> sx      == tsum y' && x  == y'
  EQ                   -> sx      == sy      && x  == y
  GT | x' <- trim ny x -> tsum x' == sy      && x' == y
  where
    x = sids a
    b = sids b
    Tally nx sx = measure x
    Tally ny sy = measure y
    trim n = fst . split ((n <) . tallyCount)
    tsum = tallySum . measure

-- *** Strand Ids

instance HasSids Path where
  sids s = FingerTree.singleton $! sid s

instance HasSids Hunk where
  sids s = FingerTree.singleton $! sid s


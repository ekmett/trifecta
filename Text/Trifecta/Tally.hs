{-# LANGUAGE DeriveDataTypeable, MultiParamTypeClasses #-}
module Text.Trifecta.Tally 
  ( Tally(..)
  , HasTally(..)
  ) where

import Data.Monoid
import Data.Semigroup
import Data.Semigroup.Reducer
import Data.Data
import Data.Hashable

data Tally = Tally 
  { tallyCount :: {-# UNPACK #-} !Int
  , tallySum   :: {-# UNPACK #-} !Int 
  } deriving (Eq, Ord, Show, Read, Data, Typeable)

instance Semigroup Tally where
  Tally a x <> Tally b y = Tally (a + b) (x + y)
  replicate1p n (Tally a x) = let n' = fromIntegral n + 1 in n' `seq` Tally (n' * a) (n' * x)

instance Hashable Tally where
  hash (Tally a x) = hashWithSalt a x

instance Monoid Tally where
  Tally a x `mappend` Tally b y = Tally (a + b) (x + y)
  mempty = Tally 0 0

instance Reducer Int Tally where
  unit = Tally 1
  cons x (Tally n y) = Tally (n + 1) (x + y)
  snoc (Tally n x) y = Tally (n + 1) (x + y)

class HasTally t where
  tally :: t -> Tally
 
instance HasTally Tally where
  tally = id

instance Measured Tally a => HasTally (FingerTree Tally a) where
  tally = measure

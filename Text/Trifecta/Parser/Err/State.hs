module Text.Trifecta.Parser.Err.State 
  ( ErrState(..)
  ) where

import Data.Functor.Plus
import Data.Set as Set
import Data.Sequence as Seq
import Data.Semigroup
import Data.Monoid
import Text.Trifecta.Parser.Err
import Text.Trifecta.Diagnostic

data ErrState e = ErrState
 { errExpected :: !(Set String)
 , errMessage  :: !(Err e)
 , errLog      :: !(Seq (Diagnostic e))
 }

instance Functor ErrState where
  fmap f (ErrState a b c) = ErrState a (fmap f b) (fmap (fmap f) c)

instance Alt ErrState where
  ErrState a b c <!> ErrState a' b' c' = ErrState (a <> a') (b <!> b') (c <!> c')
  {-# INLINE (<!>) #-}

instance Plus ErrState where
  zero = ErrState mempty zero zero
 
instance Semigroup (ErrState e) where
  (<>) = (<!>) 

instance Monoid (ErrState e) where
  mempty = zero
  mappend = (<!>)

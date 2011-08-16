module Text.Trifecta.Diagnostic.Err.Log
  ( ErrLog(..)
  ) where

import Data.Functor.Plus
import Data.Semigroup
import Text.PrettyPrint.Free
import Text.Trifecta.Diagnostic.Prim
import Text.Trifecta.Parser.Token.Highlight
import Data.IntervalMap.FingerTree as IntervalMap
import Data.Sequence (Seq)

data ErrLog e = ErrLog
  { errLog        :: !(Seq (Diagnostic e))
  , errHighlights :: !(IntervalMap (Int, Int) TokenHighlight) 
  }

instance Functor ErrLog where
  fmap f (ErrLog a b) = ErrLog (fmap (fmap f) a) b

instance Alt ErrLog where
  ErrLog a b <!> ErrLog a' b' = ErrLog (a <> a') (IntervalMap.union b b')
  {-# INLINE (<!>) #-}

instance Plus ErrLog where
  zero = ErrLog mempty IntervalMap.empty
 
instance Semigroup (ErrLog e) where
  (<>) = (<!>) 

instance Monoid (ErrLog e) where
  mempty = zero
  mappend = (<!>)

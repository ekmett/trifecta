{-# LANGUAGE FlexibleContexts #-}
module Text.Trifecta.Parser.Step 
  ( Step(..)
  , feed
  , starve
  , stepResult
  ) where

import Data.Bifunctor
import Data.Semigroup.Reducer
import Data.Sequence
import Text.Trifecta.Rope.Prim
import Text.Trifecta.Diagnostic.Prim
import Text.Trifecta.Parser.Result

data Step e a
  = StepDone !Rope !(Seq (Diagnostic e)) a
  | StepFail !Rope !(Seq (Diagnostic e))
  | StepCont !Rope (Result e a) (Rope -> Step e a)

instance (Show e, Show a) => Show (Step e a) where
  showsPrec d (StepDone r xs a) = showParen (d > 10) $ 
    showString "StepDone " . showsPrec 11 r . showChar ' ' . showsPrec 11 xs . showChar ' ' . showsPrec 11 a
  showsPrec d (StepFail r xs) = showParen (d > 10) $ 
    showString "StepFail " . showsPrec 11 r . showChar ' ' . showsPrec 11 xs
  showsPrec d (StepCont r fin _) = showParen (d > 10) $ 
    showString "StepCont " . showsPrec 11 r . showChar ' ' . showsPrec 11 fin . showString " ..."
    
instance Functor (Step e) where
  fmap f (StepDone r xs a) = StepDone r xs (f a)
  fmap _ (StepFail r xs)   = StepFail r xs
  fmap f (StepCont r z k)  = StepCont r (fmap f z) (fmap f . k)

instance Bifunctor Step where
  bimap f g (StepDone r xs a) = StepDone r (fmap (fmap f) xs) (g a)
  bimap f _ (StepFail r xs)   = StepFail r (fmap (fmap f) xs)
  bimap f g (StepCont r z k)  = StepCont r (bimap f g z) (bimap f g . k)

feed :: Reducer t Rope => t -> Step e r -> Step e r
feed t (StepDone r xs a) = StepDone (snoc r t) xs a
feed t (StepFail r xs)   = StepFail (snoc r t) xs
feed t (StepCont r _ k)  = k (snoc r t)

starve :: Step e a -> Result e a
starve (StepDone _ xs a) = Success xs a
starve (StepFail _ xs)   = Failure xs
starve (StepCont _ z _)  = z

stepResult :: Rope -> Result e a -> Step e a
stepResult r (Success xs a) = StepDone r xs a
stepResult r (Failure xs) = StepFail r xs


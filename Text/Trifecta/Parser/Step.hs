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
import Text.Trifecta.Rope
import Text.Trifecta.Diagnostic
import Text.Trifecta.Parser.Result

data Step e a
  = StepDone !Rope !(Seq (Diagnostic e)) a
  | StepFail !Rope !(Seq (Diagnostic e)) !(Diagnostic e)
  | StepCont !Rope (Result e a) (Rope -> Step e a)

instance (Show e, Show a) => Show (Step e a) where
  showsPrec d (StepDone r xs a) = showParen (d > 10) $ 
    showString "StepDone " . showsPrec 11 r . showChar ' ' . showsPrec 11 xs . showChar ' ' . showsPrec 11 a
  showsPrec d (StepFail r xs e) = showParen (d > 10) $ 
    showString "StepFail " . showsPrec 11 r . showChar ' ' . showsPrec 11 xs . showChar ' ' . showsPrec 11 e
  showsPrec d (StepCont r fin _) = showParen (d > 10) $ 
    showString "StepCont " . showsPrec 11 r . showChar ' ' . showsPrec 11 fin . showString " ..."
    
instance Functor (Step e) where
  fmap f (StepDone r xs a) = StepDone r xs (f a)
  fmap _ (StepFail r xs e) = StepFail r xs e
  fmap f (StepCont r z k)  = StepCont r (fmap f z) (fmap f . k)

instance Bifunctor Step where
  bimap f g (StepDone r xs a) = StepDone r (fmap (fmap f) xs) (g a)
  bimap f _ (StepFail r xs e) = StepFail r (fmap (fmap f) xs) (fmap f e)
  bimap f g (StepCont r z k)  = StepCont r (bimap f g z) (bimap f g . k)

feed :: Reducer t Rope => Step e r -> t -> Step e r
feed (StepDone r xs a) t = StepDone (snoc r t) xs a
feed (StepFail r xs e) t = StepFail (snoc r t) xs e
feed (StepCont r _ k) t = k (snoc r t)

starve :: Step e a -> Result e a
starve (StepDone _ xs a) = Success xs a
starve (StepFail _ xs e) = Failure xs e
starve (StepCont _ z _)  = z

stepResult :: Rope -> Result e a -> Step e a
stepResult r (Success xs a) = StepDone r xs a
stepResult r (Failure xs e) = StepFail r xs e


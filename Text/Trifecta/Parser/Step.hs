module Text.Trifecta.Parser.Step 
  ( Step(..)
  , feed
  , eof
  , eof'
  , stepResult
  ) where

import Data.Bifunctor
import Data.Semigroup.Reducer
import Data.Sequence
import Text.Trifecta.Rope
import Text.Trifecta.Diagnostic
import Text.Trifecta.Parser.Result

data Step e a =
  = StepDone !Rope !(Seq (Diagnostic e)) a
  | StepFail !Rope !(Seq (Diagnostic e)) !(Diagnostic e)
  | StepCont !Rope (Result e a) (Rope -> Step e a)

instance Functor (Step e a) where
  fmap f (StepDone r xs a) = StepDone r xs (f a)
  fmap _ (StepFail r xs e) = StepFail r xs e
  fmap f (StepCont r z k)  = StepCont r (fmap f z) (fmap f . k)

instance Bifunctor (Step e a) where
  bimap f g (StepDone r xs a) = StepDone r (fmap (fmap f) xs) (g a)
  bimap f _ (StepFail r xs e) = StepFail r (fmap (fmap f) xs) (fmap f a)
  bimap f g (StepCont r z k)  = StepCont r (bimap f g z) (bimap f g . k)

feed :: Reducer t Rope => Step e r -> t -> Step e r
feed (StepDone r xs a) t = StepDone (snoc r t) xs a
feed (StepFail r xs e) t = StepFail (snoc r t) xs e
feed (StepCont r _ k) t = k (snoc r t)

eof :: Step e a -> Result e a
eof (StepDone r xs a) = Success xs a
eof (StepFail r xs a) = Failure xs e
eof (StepCont _ z _)  = z

eof' :: Step e a -> Result e (Rope, a)
eof' (StepDone r xs a) = Success xs (r, a)
eof' (StepFail r xs a) = Failure xs e
eof' (StepCont r z _)  = fmap ((,) r) z

stepResult :: Rope -> Result e a -> Step e a
stepResult r (Success xs a) = StepDone r xs a
stepResult r (Failure xs e) = StepFail r xs e


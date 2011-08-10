module Text.Trifecta.Parser.Err
  ( Err(..)
  , diagnose
  , diagnose0
  ) where

import Data.Bifunctor
import Data.Semigroup
import Data.Monoid
import Data.Functor.Plus
import Text.Trifecta.Render.Prim
import Text.Trifecta.Diagnostic

-- | unlocated error messages
data Err e
  = EmptyErr
  | FailErr String 
  | EndOfFileErr
  | RichErr (Either Delta Render -> Diagnostic e)

diagnose :: Render -> Err (Doc e) -> Diagnostic (Doc e)
diagnose r EmptyErr     = Diagnostic r Error (text "unexpected") []
diagnose r (FailErr m)  = Diagnostic r Error (map text (words m)) []
diagnose r EndOfFileErr = Diagnostic r Error (text "unexpected EOF") []
diagnose r (RichErr f)  = f r 

diagnose0 :: Err (Doc e) -> Diagnostic (Doc e)
diagnose0 = diagnose (Left mempty)

instance Pretty t => Pretty (Err t) where
  pretty = pretty . diagnose0
  prettyList = prettyList . map diagnose0

instance PrettyTerm t => Pretty (Err t) where
  prettyTerm = prettyTerm . diagnose0
  prettyTermList = prettyTermList . map diagnose0
  
instance Pretty t => Show (Err t) where
  show = show . pretty

instance Functor Err where
  fmap _ EmptyErr       = EmptyErr
  fmap _ (FailErr s)    = FailErr s
  fmap _ EndOfFileErr            = EndOfFileErr
  fmap f (RichErr k)    = RichErr (fmap f . k)

instance Alt Err where
  EmptyErr <!> a = a
  a        <!> _ = a
  {-# INLINE (<!>) #-}

instance Plus Err where
  zero = EmptyErr

instance Semigroup (Err t) where
  (<>) = (<!>)
  replicate1p = id

instance Monoid (Err t) where
  mempty = EmptyErr
  mappend = (<!>) 

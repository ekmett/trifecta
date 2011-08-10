module Text.Trifecta.Parser.Err
  ( Err(..)
  , diagnose
  , knownErr
  ) where

import Control.Applicative
import Data.Semigroup
import Data.Monoid
import Data.Functor.Plus
import Text.Trifecta.Render.Prim
import Text.Trifecta.Diagnostic
import Text.Trifecta.Diagnostic.Level
import Text.PrettyPrint.Free
import System.Console.Terminfo.PrettyPrint

-- | unlocated error messages
data Err e
  = EmptyErr
  | FailErr String 
  | UnexpectedErr String
  | EndOfFileErr
  | RichErr (Render -> Diagnostic e)

knownErr :: Err e -> Bool
knownErr EmptyErr = False
knownErr _ = True

diagnose :: (t -> Doc e) -> Render -> Err t -> Diagnostic (Doc e)
diagnose _ r EmptyErr          = Diagnostic r Error (text "unexpected") []
diagnose _ r (FailErr m)       = Diagnostic r Error (fillSep $ text <$> words m) []
diagnose _ r (UnexpectedErr s) = Diagnostic r Error (fillSep $ fmap text $ "unexpected" : words s) []
diagnose _ r EndOfFileErr      = Diagnostic r Error (text "unexpected EOF") []
diagnose k r (RichErr f)       = fmap k (f r)

diagnose0 :: Pretty t => Err t -> Diagnostic (Doc e)
diagnose0 = diagnose pretty emptyRender

diagnoseTerm0 :: PrettyTerm t => Err t -> Diagnostic TermDoc
diagnoseTerm0 = diagnose prettyTerm emptyRender

instance Pretty t => Pretty (Err t) where
  pretty = pretty . diagnose0
  prettyList = prettyList . map diagnose0

instance PrettyTerm t => PrettyTerm (Err t) where
  prettyTerm = prettyTerm . diagnoseTerm0
  prettyTermList = prettyTermList . map diagnoseTerm0
  
instance Pretty t => Show (Err t) where
  show = show . pretty

instance Functor Err where
  fmap _ EmptyErr          = EmptyErr
  fmap _ (FailErr s)       = FailErr s
  fmap _ EndOfFileErr      = EndOfFileErr
  fmap _ (UnexpectedErr s) = UnexpectedErr s 
  fmap f (RichErr k)       = RichErr (fmap f . k)

instance Alt Err where
  EmptyErr <!> a = a
  a        <!> _ = a
  {-# INLINE (<!>) #-}

instance Plus Err where
  zero = EmptyErr

instance Semigroup (Err t) where
  (<>) = (<!>)
  replicate1p _ = id

instance Monoid (Err t) where
  mempty = EmptyErr
  mappend = (<!>) 

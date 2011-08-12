module Text.Trifecta.Diagnostic.Err
  ( Err(..)
  , diagnose
  , knownErr
  , fatalErr
  ) where

import Control.Applicative
import Control.Comonad
import Data.Semigroup
import Data.Monoid
import Data.Functor.Plus
import Text.Trifecta.Diagnostic.Prim
import Text.Trifecta.Diagnostic.Level
import Text.Trifecta.Diagnostic.Rendering.Prim
import Text.PrettyPrint.Free
import System.Console.Terminfo.PrettyPrint

-- | unlocated error messages
data Err e
  = EmptyErr
  | UnexpectedErr String
  | EndOfFileErr
  | FailErr String 
  | RichErr (Rendering -> Diagnostic e) -- relocates with us as we try
  | FatalErr (Diagnostic e)

instance Show e => Show (Err e) where
  showsPrec _ EmptyErr = showString "EmptyErr"
  showsPrec d (FailErr s) = showParen (d > 10) $
    showString "FailErr " . showsPrec 11 s
  showsPrec d (UnexpectedErr s) = showParen (d > 10) $
    showString "UnexpectedErr " . showsPrec 11 s
  showsPrec d (FatalErr e) = showParen (d > 10) $ showString "FatalErr " . showsPrec 11 e
  showsPrec _ EndOfFileErr = showString "EndOfFileErr"
  showsPrec d (RichErr _) = showParen (d > 10) $ 
    showString "RichErr ..."

knownErr :: Err e -> Bool
knownErr EmptyErr = False
knownErr _ = True

fatalErr :: Err e -> Bool
fatalErr FatalErr{} = True
fatalErr _ = False

diagnose :: (t -> Doc e) -> Rendering -> Err t -> Diagnostic (Doc e)
diagnose _ r EmptyErr          = Diagnostic r Error (text "unexpected") []
diagnose _ r (FailErr m)       = Diagnostic r Error (fillSep $ text <$> words m) []
diagnose _ r (UnexpectedErr s) = Diagnostic r Error (fillSep $ fmap text $ "unexpected" : words s) []
diagnose _ r EndOfFileErr      = Diagnostic r Error (text "unexpected EOF") []
diagnose k _ (FatalErr e)      = fmap k e
diagnose k r (RichErr f)       = fmap k (f r)

diagnose0 :: Pretty t => Err t -> Diagnostic (Doc e)
diagnose0 = diagnose pretty emptyRendering

diagnoseTerm0 :: PrettyTerm t => Err t -> Diagnostic TermDoc
diagnoseTerm0 = diagnose prettyTerm emptyRendering

instance Pretty t => Pretty (Err t) where
  pretty = pretty . extract . diagnose0
  prettyList = prettyList . map (extract . diagnose0)

instance PrettyTerm t => PrettyTerm (Err t) where
  prettyTerm = prettyTerm . diagnoseTerm0
  prettyTermList = prettyTermList . map diagnoseTerm0
  
instance Functor Err where
  fmap _ EmptyErr = EmptyErr
  fmap _ (FailErr s) = FailErr s
  fmap _ EndOfFileErr = EndOfFileErr
  fmap f (FatalErr e) = FatalErr (fmap f e)
  fmap _ (UnexpectedErr s) = UnexpectedErr s 
  fmap f (RichErr k) = RichErr (fmap f . k)

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

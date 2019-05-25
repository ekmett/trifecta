{-# language CPP                    #-}
{-# language DeriveFoldable         #-}
{-# language DeriveFunctor          #-}
{-# language DeriveTraversable      #-}
{-# language FlexibleContexts       #-}
{-# language FlexibleInstances      #-}
{-# language FunctionalDependencies #-}
{-# language MultiParamTypeClasses  #-}
{-# language Rank2Types             #-}
{-# language TemplateHaskell        #-}
{-# language UndecidableInstances   #-}
-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Edward Kmett 2011-2019
-- License     :  BSD3
--
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Results and Parse Errors
-----------------------------------------------------------------------------
module Text.Trifecta.Result
  (
  -- * Parse Results
    Result(..)
  , AsResult(..)
  , foldResult
  , _Success
  , _Failure
  -- * Parsing Errors
  , Err(..), HasErr(..), Errable(..)
  , ErrInfo(..)
  , explain
  , failed
  ) where

import           Control.Applicative                          as Alternative
import           Control.Lens                                 hiding (cons, snoc)
import           Control.Monad                                (guard)
import           Data.Foldable
import qualified Data.List                                    as List
import           Data.Maybe                                   (fromMaybe, isJust)
#if !(MIN_VERSION_base(4,11,0))
import           Data.Semigroup
#endif
import           Data.Set                                     as Set hiding (empty, toList)
import           Data.Text.Prettyprint.Doc                    as Pretty
import           Data.Text.Prettyprint.Doc.Render.Terminal    as Pretty

import Text.Trifecta.Delta       as Delta
import Text.Trifecta.Rendering
import Text.Trifecta.Util.Pretty as Pretty

data ErrInfo = ErrInfo
  { _errDoc    :: Doc AnsiStyle
  , _errDeltas :: [Delta]
  } deriving (Show)

-- | This is used to report an error. What went wrong, some supplemental docs
-- and a set of things expected at the current location. This does not, however,
-- include the actual location.
data Err = Err
  { _reason      :: Maybe (Doc AnsiStyle)
  , _footnotes   :: [Doc AnsiStyle]
  , _expected    :: Set String
  , _finalDeltas :: [Delta]
  }

makeClassy ''Err

instance Semigroup Err where
  Err md mds mes delta1 <> Err nd nds nes delta2
    = Err (nd <|> md) (if isJust nd then nds else if isJust md then mds else nds ++ mds) (mes <> nes) (delta1 <> delta2)
  {-# inlinable (<>) #-}

instance Monoid Err where
  mempty = Err Nothing [] mempty mempty
  {-# inlinable mempty #-}
  mappend = (<>)
  {-# inlinable mappend #-}

-- | Generate a simple 'Err' word-wrapping the supplied message.
failed :: String -> Err
failed m = Err (Just (fillSep (pretty <$> words m))) [] mempty mempty
{-# inlinable failed #-}

-- | Convert a 'Rendering' of auxiliary information and an 'Err' into a 'Doc AnsiStyle',
-- ready to be prettyprinted to the user.
explain :: Rendering -> Err -> Doc AnsiStyle
explain r (Err mm as es _)
  | Set.null es = report (withEx mempty)
  | isJust mm   = report $ withEx $ Pretty.char ',' <+> expecting
  | otherwise   = report expecting
  where
    now = spaceHack $ toList es
    spaceHack [""] = ["space"]
    spaceHack xs = List.filter (/= "") xs
    withEx x = fromMaybe (fillSep $ pretty <$> words "unspecified error") mm <> x
    expecting = pretty "expected:" <+> fillSep (punctuate (Pretty.char ',') (pretty <$> now))
    report txt = vsep $ [prettyDelta (delta r) <> Pretty.char ':' <+> annotate (Pretty.color Pretty.Red) (pretty "error") <> Pretty.char ':' <+> nest 4 txt]
             <|> prettyRendering r <$ guard (not (nullRendering r))
             <|> as

class Errable m where
  raiseErr :: Err -> m a

instance Monoid ErrInfo where
  mempty = ErrInfo mempty mempty
  mappend = (<>)

instance Semigroup ErrInfo where
  ErrInfo xs d1 <> ErrInfo ys d2 = ErrInfo (vsep [xs, ys]) (max d1 d2)

-- | The result of parsing. Either we succeeded or something went wrong.
data Result a
  = Success a
  | Failure ErrInfo
  deriving (Show,Functor,Foldable,Traversable)

-- | Fold over a 'Result'
foldResult :: (ErrInfo -> b) -> (a -> b) -> Result a -> b
foldResult f g r = case r of
  Failure e -> f e
  Success a -> g a

-- | A 'Prism' that lets you embed or retrieve a 'Result' in a potentially larger type.
class AsResult s t a b | s -> a, t -> b, s b -> t, t a -> s where
  _Result :: Prism s t (Result a) (Result b)

instance AsResult (Result a) (Result b) a b where
  _Result = id
  {-# inlinable _Result #-}

-- | The 'Prism' for the 'Success' constructor of 'Result'
_Success :: AsResult s t a b => Prism s t a b
_Success = _Result . dimap seta (either id id) . right' . rmap (fmap Success) where
  seta (Success a) = Right a
  seta (Failure e) = Left (pure (Failure e))
{-# inlinable _Success #-}

-- | The 'Prism' for the 'Failure' constructor of 'Result'
_Failure :: AsResult s s a a => Prism' s ErrInfo
_Failure = _Result . dimap seta (either id id) . right' . rmap (fmap Failure) where
  seta (Failure e) = Right e
  seta (Success a) = Left (pure (Success a))
{-# inlinable _Failure #-}

instance Applicative Result where
  pure = Success
  {-# inlinable pure #-}
  Success f <*> Success a = Success (f a)
  Success _ <*> Failure y = Failure y
  Failure x <*> Success _ = Failure x
  Failure x <*> Failure y =
    Failure $ ErrInfo (vsep [_errDoc x, _errDoc y]) (_errDeltas x <> _errDeltas y)
  {-# inlinable (<*>) #-}

instance Alternative Result where
  Failure x <|> Failure y =
    Failure $ ErrInfo (vsep [_errDoc x, _errDoc y]) (_errDeltas x <> _errDeltas y)
  Success a <|> Success _ = Success a
  Success a <|> Failure _ = Success a
  Failure _ <|> Success a = Success a
  {-# inlinable (<|>) #-}
  empty = Failure mempty
  {-# inlinable empty #-}

instance Monad Result where
  return = pure
  Success a >>= m = m a
  Failure e >>= _ = Failure e

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Trifecta.Result
-- Copyright   :  (c) Edward Kmett 2011-2013
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
  , _Success
  , _Failure
  -- * Parsing Errors
  , Err(..), HasErr(..), Errable(..)
  , explain
  , failed
  ) where

import Control.Applicative as Alternative
import Control.Lens hiding (snoc, cons)
import Control.Monad (guard)
import Data.Foldable
import Data.Maybe (fromMaybe, isJust)
import qualified Data.List as List
import Data.Semigroup
import Data.Set as Set hiding (empty, toList)
import Text.PrettyPrint.ANSI.Leijen as Pretty hiding (line, (<>), (<$>), empty)
import Text.Trifecta.Instances ()
import Text.Trifecta.Rendering
import Text.Trifecta.Delta as Delta

-- | This is used to report an error. What went wrong, some supplemental docs and a set of things expected
-- at the current location. This does not, however, include the actual location.
data Err = Err
  { _reason    :: Maybe Doc
  , _footnotes :: [Doc]
  , _expected  :: Set String
  }

makeClassy ''Err

instance Semigroup Err where
  Err md mds mes <> Err nd nds nes
    = Err (nd <|> md) (if isJust nd then nds else if isJust md then mds else nds ++ mds) (mes <> nes)
  {-# INLINE (<>) #-}

instance Monoid Err where
  mempty = Err Nothing [] mempty
  {-# INLINE mempty #-}
  mappend = (<>)
  {-# INLINE mappend #-}

-- | Generate a simple 'Err' word-wrapping the supplied message.
failed :: String -> Err
failed m = Err (Just (fillSep (pretty <$> words m))) [] mempty
{-# INLINE failed #-}

-- | Convert a location and an 'Err' into a 'Doc'
explain :: Rendering -> Err -> Doc
explain r (Err mm as es)
  | Set.null es = report (withEx mempty)
  | isJust mm   = report $ withEx $ Pretty.char ',' <+> expecting
  | otherwise   = report expecting
  where
    now = spaceHack $ List.nub $ toList es
    spaceHack [""] = ["space"]
    spaceHack xs = List.filter (/= "") xs
    withEx x = fromMaybe (fillSep $ text <$> words "unspecified error") mm <> x
    expecting = text "expected:" <+> fillSep (punctuate (Pretty.char ',') (text <$> now))
    report txt = vsep $ [pretty (delta r) <> Pretty.char ':' <+> red (text "error") <> Pretty.char ':' <+> nest 4 txt]
             <|> pretty r <$ guard (not (nullRendering r))
             <|> as

class Errable m where
  raiseErr :: Err -> m a

-- | The result of parsing. Either we succeeded or something went wrong.
data Result a
  = Success a
  | Failure Doc
  deriving (Show,Functor,Foldable,Traversable)

-- | A 'Prism' that lets you embed or retrieve a 'Result' in a potentially larger type.
class AsResult s t a b | s -> a, t -> b, s b -> t, t a -> s where
  _Result :: Prism s t (Result a) (Result b)

instance AsResult (Result a) (Result b) a b where
  _Result = id
  {-# INLINE _Result #-}

-- | The 'Prism' for the 'Success' constructor of 'Result'
_Success :: AsResult s t a b => Prism s t a b
_Success = _Result . dimap seta (either id id) . right' . rmap (fmap Success) where
  seta (Success a) = Right a
  seta (Failure d) = Left (pure (Failure d))
{-# INLINE _Success #-}

-- | The 'Prism' for the 'Failure' constructor of 'Result'
_Failure :: AsResult s s a a => Prism' s Doc
_Failure = _Result . dimap seta (either id id) . right' . rmap (fmap Failure) where
  seta (Failure d) = Right d
  seta (Success a) = Left (pure (Success a))
{-# INLINE _Failure #-}

instance Show a => Pretty (Result a) where
  pretty (Success a)  = pretty (show a)
  pretty (Failure xs) = pretty xs

instance Applicative Result where
  pure = Success
  {-# INLINE pure #-}
  Success f  <*> Success a  = Success (f a)
  Success _  <*> Failure ys = Failure ys
  Failure xs <*> Success _  = Failure xs
  Failure xs <*> Failure ys = Failure $ vsep [xs, ys]
  {-# INLINE (<*>) #-}

instance Alternative Result where
  Failure xs <|> Failure ys = Failure $ vsep [xs, ys]
  Success a  <|> Success _  = Success a
  Success a  <|> Failure _  = Success a
  Failure _  <|> Success a  = Success a
  {-# INLINE (<|>) #-}
  empty = Failure mempty
  {-# INLINE empty #-}

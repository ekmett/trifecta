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
  , Err(..), HasErr(..)
  , explain
  , failing
  ) where

import Control.Applicative as Alternative
import Control.Lens hiding (snoc, cons)
import Control.Monad (guard)
import Data.Foldable
import Data.Maybe (fromMaybe, isJust)
import qualified Data.List as List
import Data.Semigroup
-- import Data.Sequence as Seq hiding (empty)
import Data.Set as Set hiding (empty, toList)
import Text.PrettyPrint.ANSI.Leijen as Pretty hiding (line, (<>), (<$>), empty)
import Text.Trifecta.Instances ()
import Text.Trifecta.Rendering
import Text.Trifecta.Delta as Delta

data Err = Err
  { _reason    :: Maybe Doc
  , _footnotes :: [Doc]
  , _expected  :: Set String
  }

makeClassy ''Err

instance Semigroup Err where
  Err md mds mes <> Err nd nds nes
    = Err (nd <|> md) (if isJust nd then nds else if isJust md then mds else nds ++ mds) (mes <> nes)

instance Monoid Err where
  mempty = Err Nothing [] mempty
  mappend = (<>)

failing :: String -> Err
failing m = Err (Just (fillSep (pretty <$> words m))) [] mempty

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

data Result a
  = Success a
  | Failure Doc
  deriving (Show,Functor,Foldable,Traversable)

class AsResult p f s t a b | s -> a, t -> b, s b -> t, t a -> s where
  _Result :: Overloaded p f s t (Result a) (Result b)

instance AsResult p f (Result a) (Result b) a b where
  _Result = id
  {-# INLINE _Result #-}

_Success :: (AsResult p f s t a b, Choice p, Applicative f) => Overloaded p f s t a b
_Success = _Result . dimap seta (either id id) . right' . rmap (fmap Success) where
  seta (Success a) = Right a
  seta (Failure d) = Left (pure (Failure d))
{-# INLINE _Success #-}

_Failure :: (AsResult p f s s a a, Choice p, Applicative f) => Overloaded' p f s Doc
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

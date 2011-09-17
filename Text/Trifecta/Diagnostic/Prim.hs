{-# LANGUAGE FlexibleContexts, DeriveDataTypeable #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Trifecta.Diagnostic.Prim
-- Copyright   :  (C) 2011 Edward Kmett,
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Rich diagnostics
----------------------------------------------------------------------------
module Text.Trifecta.Diagnostic.Prim
  ( Diagnostic(..)
  ) where

import Control.Applicative
import Control.Comonad
import Control.Monad (guard)
import Control.Exception
import Control.Monad.Writer.Class
import Data.Functor.Apply
import Data.Foldable
import Data.Traversable
import Data.List.NonEmpty hiding (map)
import Data.Semigroup
import Data.Semigroup.Reducer
import Data.Semigroup.Foldable
import Data.Semigroup.Traversable
import Text.Trifecta.Rope.Bytes
import Text.Trifecta.Rope.Delta
import Text.Trifecta.Diagnostic.Rendering.Prim
import Text.Trifecta.Diagnostic.Level
import Text.PrettyPrint.Free
import Text.Trifecta.Highlight.Class
import System.Console.Terminfo.PrettyPrint
import Prelude hiding (log)
import Data.Typeable

data Diagnostic m = Diagnostic !(Either String Rendering) !DiagnosticLevel m [Diagnostic m]
  deriving (Show, Typeable)

instance Highlightable (Diagnostic e) where
  addHighlights h (Diagnostic rs l m xs) = Diagnostic (addHighlights h <$> rs) l m (addHighlights h <$> xs)

instance (Typeable m, Show m) => Exception (Diagnostic m)

instance Renderable (Diagnostic m) where
  render (Diagnostic r _ _ _) = either (const emptyRendering) id r

instance HasDelta (Diagnostic m) where
  delta (Diagnostic r _ _ _) = either (const mempty) delta r

instance HasBytes (Diagnostic m) where
  bytes (Diagnostic r _ _ _) = either (const 0) (bytes . delta) r

instance Extend Diagnostic where
  extend f d@(Diagnostic r l _ xs) = Diagnostic r l (f d) (map (extend f) xs)

instance Comonad Diagnostic where
  extract (Diagnostic _ _ m _) = m

instance Pretty m => Pretty (Diagnostic m) where
  pretty (Diagnostic src l m xs) = case src of 
    Left p  -> vsep $ [pretty p <> msg]
                  <|> children
    Right r -> vsep $ [pretty (delta r) <> msg]
                  <|> pretty r <$ guard (not (nullRendering r))
                  <|> children
    where 
      msg = char ':' <+> pretty l <> char ':' <+> nest 4 (pretty m) 
      children = indent 2 (prettyList xs) <$ guard (not (null xs))

  prettyList = vsep . Prelude.map pretty

instance PrettyTerm m => PrettyTerm (Diagnostic m) where
  prettyTerm (Diagnostic src l m xs) = case src of 
    Left p  -> vsep $ [prettyTerm p <> msg]
                  <|> children
    Right r -> vsep $ [prettyTerm (delta r) <> msg]
                  <|> prettyTerm r <$ guard (not (nullRendering r))
                  <|> children
    where 
      msg = char ':' <+> prettyTerm l <> char ':' <+> nest 4 (prettyTerm m) 
      children = indent 2 (prettyTermList xs) <$ guard (not (null xs))
  prettyTermList = vsep . Prelude.map prettyTerm

instance Functor Diagnostic where
  fmap f (Diagnostic r l m xs) = Diagnostic r l (f m) $ map (fmap f) xs

instance Foldable Diagnostic where
  foldMap f (Diagnostic _ _ m xs) = f m `mappend` foldMap (foldMap f) xs

instance Traversable Diagnostic where
  traverse f (Diagnostic r l m xs) = Diagnostic r l <$> f m <*> traverse (traverse f) xs

instance Foldable1 Diagnostic where
  foldMap1 f (Diagnostic _ _ m []) = f m
  foldMap1 f (Diagnostic _ _ m (x:xs)) = f m <> foldMap1 (foldMap1 f) (x:|xs)

instance Traversable1 Diagnostic where
  traverse1 f (Diagnostic r l m [])     = fmap (\fm -> Diagnostic r l fm []) (f m)
  traverse1 f (Diagnostic r l m (x:xs)) = (\fm (y:|ys) -> Diagnostic r l fm (y:ys)) 
                                      <$> f m 
                                      <.> traverse1 (traverse1 f) (x:|xs)

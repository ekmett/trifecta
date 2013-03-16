{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Trifecta.Rendering
-- Copyright   :  (C) 2011-2013 Edward Kmett,
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- The type for Lines will very likely change over time, to enable drawing
-- lit up multi-character versions of control characters for @^Z@, @^[@,
-- @<0xff>@, etc. This will make for much nicer diagnostics when
-- working with protocols.
--
----------------------------------------------------------------------------
module Text.Trifecta.Rendering
  ( Rendering(Rendering)
  , HasRendering(..)
  , nullRendering
  , emptyRendering
  , Source(..)
  , rendered
  , Renderable(..)
  , Rendered(..)
  -- * Carets
  , Caret(..)
  , HasCaret(..)
  , Careted(..)
  , drawCaret
  , addCaret
  , caretEffects
  , renderingCaret
  -- * Spans
  , Span(..)
  , HasSpan(..)
  , Spanned(..)
  , spanEffects
  , drawSpan
  , addSpan
  -- * Fixits
  , Fixit(..)
  , HasFixit(..)
  , drawFixit
  , addFixit
  -- * Drawing primitives
  , Lines
  , draw
  , ifNear
  , (.#)
  ) where

import Control.Applicative
import Control.Comonad
import Control.Lens
import Data.Array
import Data.Text as B hiding (groupBy, empty, any)
import Data.Data
import Data.Foldable
import Data.Function (on)
import Data.Hashable
import Data.Int (Int64)
import Data.Maybe
import Data.List (groupBy)
import Data.Semigroup
import Data.Semigroup.Reducer
import GHC.Generics
import Prelude as P hiding (span)
import System.Console.ANSI
import Text.PrettyPrint.ANSI.Leijen hiding (column, (<>), (<$>))
import Text.Trifecta.Delta
import Text.Trifecta.Instances ()
import Text.Trifecta.Util.Combinators

outOfRangeEffects :: [SGR] -> [SGR]
outOfRangeEffects xs = SetConsoleIntensity BoldIntensity : xs

sgr :: [SGR] -> Doc -> Doc
sgr xs0 = go (P.reverse xs0) where
  go []                                         = id
  go (SetConsoleIntensity NormalIntensity : xs) = debold . go xs
  go (SetConsoleIntensity BoldIntensity   : xs) = bold . go xs
  go (SetUnderlining NoUnderline          : xs) = deunderline . go xs
  go (SetUnderlining SingleUnderline      : xs) = underline . go xs
  go (SetColor f i c                      : xs) = case f of
    Foreground -> case i of
      Dull -> case c of
        Black   -> dullblack . go xs
        Red     -> dullred . go xs
        Green   -> dullgreen . go xs
        Yellow  -> dullyellow . go xs
        Blue    -> dullblue . go xs
        Magenta -> dullmagenta . go xs
        Cyan    -> dullcyan . go xs
        White   -> dullwhite . go xs
      Vivid -> case c of
        Black   -> black . go xs
        Red     -> red . go xs
        Green   -> green . go xs
        Yellow  -> yellow . go xs
        Blue    -> blue . go xs
        Magenta -> magenta . go xs
        Cyan    -> cyan . go xs
        White   -> white . go xs
    Background -> case i of
      Dull -> case c of
        Black   -> ondullblack . go xs
        Red     -> ondullred . go xs
        Green   -> ondullgreen . go xs
        Yellow  -> ondullyellow . go xs
        Blue    -> ondullblue . go xs
        Magenta -> ondullmagenta . go xs
        Cyan    -> ondullcyan . go xs
        White   -> ondullwhite . go xs
      Vivid -> case c of
        Black   -> onblack . go xs
        Red     -> onred . go xs
        Green   -> ongreen . go xs
        Yellow  -> onyellow . go xs
        Blue    -> onblue . go xs
        Magenta -> onmagenta . go xs
        Cyan    -> oncyan . go xs
        White   -> onwhite . go xs
  go (_                                   : xs) = go xs

type Lines = Array (Int,Int64) ([SGR], Char)

(///) :: Ix i => Array i e -> [(i, e)] -> Array i e
a /// xs = a // P.filter (inRange (bounds a) . fst) xs

grow :: Int -> Lines -> Lines
grow y a
  | inRange (t,b) y = a
  | otherwise = array new [ (i, if inRange old i then a ! i else ([],' ')) | i <- range new ]
  where old@((t,lo),(b,hi)) = bounds a
        new = ((min t y,lo),(max b y,hi))

draw :: [SGR] -> Int -> Int64 -> String -> Lines -> Lines
draw e y n xs a0
  | P.null xs = a0
  | otherwise = gt $ lt (a /// out)
  where
    a = grow y a0
    ((_,lo),(_,hi)) = bounds a
    out = P.zipWith (\i c -> ((y,i),(e,c))) [n..] xs
    lt | P.any (\el -> snd (fst el) < lo) out = (// [((y,lo),(outOfRangeEffects e,'<'))])
       | otherwise = id
    gt | P.any (\el -> snd (fst el) > hi) out = (// [((y,hi),(outOfRangeEffects e,'>'))])
       | otherwise = id

data Rendering = Rendering
  { _renderingDelta    :: !Delta                 -- focus, the render will keep this visible
  , _renderingLineLen   :: {-# UNPACK #-} !Int64 -- actual line length
  , _renderingLineUnits :: {-# UNPACK #-} !Int64 -- line length in units
  , _renderingLine     :: Lines -> Lines
  , _renderingOverlays :: Delta -> Lines -> Lines
  }

makeClassy ''Rendering

instance Show Rendering where
  showsPrec d (Rendering p ll lb _ _) = showParen (d > 10) $
    showString "Rendering " . showsPrec 11 p . showChar ' ' . showsPrec 11 ll . showChar ' ' . showsPrec 11 lb . showString " ... ..."

nullRendering :: Rendering -> Bool
nullRendering (Rendering (Columns 0 0) 0 0 _ _) = True
nullRendering _ = False

emptyRendering :: Rendering
emptyRendering = Rendering (Columns 0 0) 0 0 id (const id)

instance Semigroup Rendering where
  -- an unprincipled hack
  Rendering (Columns 0 0) 0 0 _ f <> Rendering del len lb dc g = Rendering del len lb dc $ \d l -> f d (g d l)
  Rendering del len lb dc f <> Rendering _ _ _ _ g = Rendering del len lb dc $ \d l -> f d (g d l)

instance Monoid Rendering where
  mappend = (<>)
  mempty = emptyRendering

ifNear :: Delta -> (Lines -> Lines) -> Delta -> Lines -> Lines
ifNear d f d' l | near d d' = f l
                | otherwise = l

instance HasDelta Rendering where
  delta = _renderingDelta

class Renderable t where
  render :: t -> Rendering

instance Renderable Rendering where
  render = id

class Source t where
  source :: t -> (Int64, Int64, Lines -> Lines) {- the number of (padded) columns, number of units, and the the line -}

instance Source String where
  source s
    | P.elem '\n' s = ( ls, bs, draw [] 0 0 s')
    | otherwise           = ( ls + fromIntegral (P.length end), bs, draw [SetColor Foreground Vivid Blue, SetConsoleIntensity BoldIntensity] 0 ls end . draw [] 0 0 s')
    where
      end = "<EOF>"
      s' = go 0 s
      bs = fromIntegral $ B.length $ pack $ P.takeWhile (/='\n') s
      ls = fromIntegral $ P.length s'
      go n ('\t':xs) = let t = 8 - mod n 8 in P.replicate t ' ' ++ go (n + t) xs
      go _ ('\n':_)  = []
      go n (x:xs)    = x : go (n + 1) xs
      go _ []        = []


instance Source Text where
  source = source . unpack

-- | create a drawing surface
rendered :: Source s => Delta -> s -> Rendering
rendered del s = case source s of
  (len, lb, dc) -> Rendering del len lb dc (\_ l -> l)

(.#) :: (Delta -> Lines -> Lines) -> Rendering -> Rendering
f .# Rendering d ll lb s g = Rendering d ll lb s $ \e l -> f e $ g e l

instance Pretty Rendering where
  pretty (Rendering d ll _ l f) = nesting $ \k -> columns $ \mn -> go (fromIntegral (fromMaybe 80 mn - k)) where
    go cols = align (vsep (P.map ln [t..b])) where
      (lo, hi) = window (column d) ll (min (max (cols - 2) 30) 200)
      a = f d $ l $ array ((0,lo),(-1,hi)) []
      ((t,_),(b,_)) = bounds a
      ln y = hcat
           $ P.map (\g -> sgr (fst (P.head g)) (pretty (P.map snd g)))
           $ groupBy ((==) `on` fst)
           [ a ! (y,i) | i <- [lo..hi] ]

window :: Int64 -> Int64 -> Int64 -> (Int64, Int64)
window c l w
  | c <= w2     = (0, min w l)
  | c + w2 >= l = if l > w then (l-w, l) else (0, w)
  | otherwise   = (c-w2,c + w2)
  where w2 = div w 2

data Rendered a = a :@ Rendering
  deriving Show

instance Functor Rendered where
  fmap f (a :@ s) = f a :@ s

instance HasDelta (Rendered a) where
  delta = delta . render

instance HasUnits (Rendered a) where
  units = units . delta

instance Comonad Rendered where
  extend f as@(_ :@ s) = f as :@ s
  extract (a :@ _) = a

instance ComonadApply Rendered where
  (f :@ s) <@> (a :@ t) = f a :@ (s <> t)

instance Foldable Rendered where
  foldMap f (a :@ _) = f a

instance Traversable Rendered where
  traverse f (a :@ s) = (:@ s) <$> f a

instance Renderable (Rendered a) where
  render (_ :@ s) = s

-- |
-- > In file included from baz.c:9
-- > In file included from bar.c:4
-- > foo.c:8:36: note
-- > int main(int argc, char ** argv) { int; }
-- >                                    ^
data Caret = Caret !Delta {-# UNPACK #-} !Text deriving (Eq,Ord,Show,Data,Typeable,Generic)

class HasCaret t where
  caret :: Lens' t Caret

instance HasCaret Caret where
  caret = id

instance Hashable Caret

caretEffects :: [SGR]
caretEffects = [SetColor Foreground Vivid Green]

drawCaret :: Delta -> Delta -> Lines -> Lines
drawCaret p = ifNear p $ draw caretEffects 1 (fromIntegral (column p)) "^"

addCaret :: Delta -> Rendering -> Rendering
addCaret p r = drawCaret p .# r

instance HasUnits Caret where
  units = units . delta

instance HasDelta Caret where
  delta (Caret d _) = d

instance Renderable Caret where
  render (Caret d bs) = addCaret d $ rendered d bs

instance Reducer Caret Rendering where
  unit = render

instance Semigroup Caret where
  a <> _ = a

renderingCaret :: Delta -> Text -> Rendering
renderingCaret d bs = addCaret d $ rendered d bs

data Careted a = a :^ Caret deriving (Eq,Ord,Show,Data,Typeable,Generic)

instance HasCaret (Careted a) where
  caret f (a :^ c) = (a :^) <$> f c

instance Functor Careted where
  fmap f (a :^ s) = f a :^ s

instance HasDelta (Careted a) where
  delta (_ :^ c) = delta c

instance HasUnits (Careted a) where
  units (_ :^ c) = units c

instance Comonad Careted where
  extend f as@(_ :^ s) = f as :^ s
  extract (a :^ _) = a

instance ComonadApply Careted where
  (a :^ c) <@> (b :^ d) = a b :^ (c <> d)

instance Foldable Careted where
  foldMap f (a :^ _) = f a

instance Traversable Careted where
  traverse f (a :^ s) = (:^ s) <$> f a

instance Renderable (Careted a) where
  render (_ :^ a) = render a

instance Reducer (Careted a) Rendering where
  unit = render

instance Hashable a => Hashable (Careted a)

spanEffects :: [SGR]
spanEffects  = [SetColor Foreground Dull Green]

drawSpan :: Delta -> Delta -> Delta -> Lines -> Lines
drawSpan s e d a
  | nl && nh  = go (column l) (rep (max (column h - column l) 0) '~') a
  | nl        = go (column l) (rep (max (snd (snd (bounds a)) - column l + 1) 0) '~') a
  |       nh  = go (-1)       (rep (max (column h + 1) 0) '~') a
  | otherwise = a
  where
    go = draw spanEffects 1 . fromIntegral
    l = argmin units s e
    h = argmax units s e
    nl = near l d
    nh = near h d
    rep = P.replicate . fromIntegral

-- |
-- > int main(int argc, char ** argv) { int; }
-- >                                    ^~~
addSpan :: Delta -> Delta -> Rendering -> Rendering
addSpan s e r = drawSpan s e .# r

data Span = Span !Delta !Delta {-# UNPACK #-} !Text deriving (Eq,Ord,Show,Data,Typeable,Generic)

class HasSpan t where
  span :: Lens' t Span

instance HasSpan Span where
  span = id

instance Renderable Span where
  render (Span s e bs) = addSpan s e $ rendered s bs

instance Semigroup Span where
  Span s _ b <> Span _ e _ = Span s e b

instance Reducer Span Rendering where
  unit = render

instance Hashable Span

data Spanned a = a :~ Span deriving (Eq,Ord,Show,Data,Typeable,Generic)

instance HasSpan (Spanned a) where
  span f (a :~ c) = (a :~) <$> f c

instance Functor Spanned where
  fmap f (a :~ s) = f a :~ s

instance Comonad Spanned where
  extend f as@(_ :~ s) = f as :~ s
  extract (a :~ _) = a

instance ComonadApply Spanned where
  (a :~ c) <@> (b :~ d) = a b :~ (c <> d)

instance Foldable Spanned where
  foldMap f (a :~ _) = f a

instance Traversable Spanned where
  traverse f (a :~ s) = (:~ s) <$> f a

instance Reducer (Spanned a) Rendering where
  unit = render

instance Renderable (Spanned a) where
  render (_ :~ s) = render s

instance Hashable a => Hashable (Spanned a)

-- > int main(int argc char ** argv) { int; }
-- >                  ^
-- >                  ,
drawFixit :: Delta -> Delta -> String -> Delta -> Lines -> Lines
drawFixit s e rpl d a = ifNear l (draw [SetColor Foreground Dull Blue] 2 (fromIntegral (column l)) rpl) d
                      $ drawSpan s e d a
  where l = argmin units s e

addFixit :: Delta -> Delta -> String -> Rendering -> Rendering
addFixit s e rpl r = drawFixit s e rpl .# r

data Fixit = Fixit
  { _fixitSpan        :: {-# UNPACK #-} !Span
  , _fixitReplacement :: !Text
  } deriving (Eq,Ord,Show,Data,Typeable,Generic)

makeClassy ''Fixit

instance HasSpan Fixit where
  span = fixitSpan

instance Hashable Fixit

instance Reducer Fixit Rendering where
  unit = render

instance Renderable Fixit where
  render (Fixit (Span s e bs) r) = addFixit s e (unpack r) $ rendered s bs

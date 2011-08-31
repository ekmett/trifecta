{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Trifecta.Diagnostic.Rendering.Prim
-- Copyright   :  (C) 2011 Edward Kmett
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
-----------------------------------------------------------------------------

module Text.Trifecta.Diagnostic.Rendering.Prim 
  ( Rendering(..)
  , nullRendering
  , emptyRendering
  , Source(..)
  , rendering
  , Renderable(..)
  , Rendered(..)
  -- * Lower level drawing primitives
  , Lines
  , draw
  , ifNear
  , (.#)
  ) where

import Control.Applicative
import Control.Comonad
import Control.Monad.State
import Data.Array
import Data.ByteString as B hiding (groupBy, empty, any)
import Data.Foldable
import Data.Function (on)
import Data.Int (Int64)
import Data.Functor.Bind
import Data.List (groupBy)
import Data.Semigroup
import Data.Semigroup.Foldable
import Data.Semigroup.Traversable
import Data.Traversable
import Text.Trifecta.IntervalMap
import Prelude as P
import Prelude hiding (span)
import System.Console.Terminfo.Color
import System.Console.Terminfo.PrettyPrint
import Text.PrettyPrint.Free hiding (column)
import Text.Trifecta.Rope.Bytes
import Text.Trifecta.Rope.Delta
import Text.Trifecta.Highlight.Class
import Text.Trifecta.Highlight.Effects
import qualified Data.ByteString.UTF8 as UTF8 

outOfRangeEffects :: [ScopedEffect] -> [ScopedEffect]
outOfRangeEffects xs = soft Bold : xs

type Lines = Array (Int,Int64) ([ScopedEffect], Char)

(///) :: Ix i => Array i e -> [(i, e)] -> Array i e
a /// xs = a // P.filter (inRange (bounds a) . fst) xs

grow :: Int -> Lines -> Lines
grow y a 
  | inRange (t,b) y = a
  | otherwise = array new [ (i, if inRange old i then a ! i else ([],' ')) | i <- range new ]
  where old@((t,lo),(b,hi)) = bounds a
        new = ((min t y,lo),(max b y,hi))

draw :: [ScopedEffect] -> Int -> Int64 -> String -> Lines -> Lines
draw e y n xs a0 
  | Prelude.null xs = a0
  | otherwise = gt $ lt (a /// out) 
  where 
    a = grow y a0
    ((_,lo),(_,hi)) = bounds a
    out = P.zipWith (\i c -> ((y,i),(e,c))) [n..] xs
    lt | Prelude.any (\el -> snd (fst el) < lo) out = (// [((y,lo),(outOfRangeEffects e,'<'))])
       | otherwise = id
    gt | Prelude.any (\el -> snd (fst el) > hi) out = (// [((y,hi),(outOfRangeEffects e,'>'))])
       | otherwise = id

-- | fill the interval from [n .. m) with a given effect
recolor :: ([ScopedEffect] -> [ScopedEffect]) -> Maybe Int64 -> Maybe Int64 -> Lines -> Lines
recolor f n0 m0 a0 
  | m <= n = a0
  | otherwise = a /// P.map rc [n .. m - 1]
  where 
    ((_,lo),(_,hi)) = bounds a
    n = maybe lo id n0
    m = maybe (hi + 1) id m0
    a = grow 0 a0
    rc i = (yi, (f e, c)) -- only if not isSpace?
      where 
        yi = (0, i)
        (e,c) = a ! yi

data Rendering = Rendering
  { renderingDelta    :: !Delta                 -- focus, the render will keep this visible
  , renderingLineLen   :: {-# UNPACK #-} !Int64 -- actual line length
  , renderingLineBytes :: {-# UNPACK #-} !Int64 -- line length in bytes
  , renderingLine     :: Lines -> Lines
  , renderingOverlays :: Delta -> Lines -> Lines
  }

instance Highlightable Rendering where
  addHighlights intervals (Rendering d ll lb l o) = Rendering d ll lb l' o where
    d' = rewind d
    l' = Prelude.foldr (.) l [ recolor (eff tok) (column lo <$ guard (near d lo)) (column hi <$ guard (near d hi)) 
                             | (Interval lo hi, tok) <- intersections d' (d' <> Columns ll lb) intervals ]
    eff t _ = highlightEffects t

instance Show Rendering where
  showsPrec d (Rendering p ll lb _ _) = showParen (d > 10) $ 
    showString "Rendering " . showsPrec 11 p . showChar ' ' . showsPrec 11 ll . showChar ' ' . showsPrec 11 lb . showString " ... ..."

nullRendering :: Rendering -> Bool
nullRendering (Rendering (Columns 0 0) 0 0 _ _) = True
nullRendering _ = False

emptyRendering :: Rendering
emptyRendering = rendering (Columns 0 0) ""

instance Semigroup Rendering where
  -- an unprincipled hack
  Rendering (Columns 0 0) 0 0 _ f <> Rendering del len lb doc g = Rendering del len lb doc $ \d l -> f d (g d l)
  Rendering del len lb doc f <> Rendering _ _ _ _ g = Rendering del len lb doc $ \d l -> f d (g d l)

instance Monoid Rendering where
  mappend = (<>) 
  mempty = emptyRendering
  
ifNear :: Delta -> (Lines -> Lines) -> Delta -> Lines -> Lines
ifNear d f d' l | near d d' = f l 
                | otherwise = l

instance HasDelta Rendering where
  delta = renderingDelta

class Renderable t where
  render :: t -> Rendering

instance Renderable Rendering where
  render = id

class Source t where
  source :: t -> (Int64, Int64, Lines -> Lines) {- the number of (padded) columns, number of bytes, and the the line -}

instance Source String where
  source s 
    | Prelude.elem '\n' s = ( ls, bs, draw [] 0 0 s') 
    | otherwise           = ( ls + fromIntegral (Prelude.length end), bs, draw [soft (Foreground Blue), soft Bold] 0 ls end . draw [] 0 0 s') 
    where
      end = "<EOF>" 
      s' = go 0 s
      bs = fromIntegral $ B.length $ UTF8.fromString $ Prelude.takeWhile (/='\n') s
      ls = fromIntegral $ Prelude.length s' 
      go n ('\t':xs) = let t = 8 - mod n 8 in P.replicate t ' ' ++ go (n + t) xs
      go _ ('\n':_)  = []
      go n (x:xs)    = x : go (n + 1) xs
      go _ []        = []
      

instance Source ByteString where
  source = source . UTF8.toString

-- | create a drawing surface
rendering :: Source s => Delta -> s -> Rendering
rendering del s = case source s of 
  (len, lb, doc) -> Rendering del len lb doc (\_ l -> l)

(.#) :: (Delta -> Lines -> Lines) -> Rendering -> Rendering
f .# Rendering d ll lb s g = Rendering d ll lb s $ \e l -> f e $ g e l 

instance Pretty Rendering where
  pretty r = prettyTerm r >>= const empty

instance PrettyTerm Rendering where
  prettyTerm (Rendering d ll _ l f) = nesting $ \k -> columns $ \n -> go (fromIntegral (n - k)) where
    go cols = align (vsep (P.map ln [t..b])) where 
      (lo, hi) = window (column d) ll (min (max (cols - 2) 30) 200)
      a = f d $ l $ array ((0,lo),(-1,hi)) []
      ((t,_),(b,_)) = bounds a
      ln y = hcat 
           $ P.map (\g -> P.foldr with (pretty (P.map snd g)) (fst (P.head g)))
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

instance HasBytes (Rendered a) where
  bytes = bytes . delta

instance Extend Rendered where
  extend f as@(_ :@ s) = f as :@ s

instance Comonad Rendered where
  extract (a :@ _) = a

instance Apply Rendered where
  (f :@ s) <.> (a :@ t) = f a :@ (s <> t)

instance Bind Rendered where
  (a :@ s) >>- f = case f a of
     b :@ t -> b :@ (s <> t)

instance Foldable Rendered where
  foldMap f (a :@ _) = f a 

instance Traversable Rendered where
  traverse f (a :@ s) = (:@ s) <$> f a

instance Foldable1 Rendered where
  foldMap1 f (a :@ _) = f a 

instance Traversable1 Rendered where
  traverse1 f (a :@ s) = (:@ s) <$> f a

instance Renderable (Rendered a) where
  render (_ :@ s) = s

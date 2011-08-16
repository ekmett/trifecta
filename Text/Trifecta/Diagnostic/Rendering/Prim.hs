{-# LANGUAGE TypeSynonymInstances #-}

-- | Diagnostics rendering
--
-- The type for Lines will very likely change over time, so we can draw lit up control 
-- characters for ^Z, ^[, <0xff>, etc. this will make for much nicer diagnostics when 
-- working with protocols!
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
import Data.ByteString hiding (groupBy, empty, any)
import Data.Foldable
import Data.Function (on)
import Data.Functor.Bind
import Data.List (groupBy)
import Data.Semigroup
import Data.Semigroup.Foldable
import Data.Semigroup.Traversable
import Data.Traversable
import Prelude as P
import Prelude hiding (span)
import System.Console.Terminfo.Color
import System.Console.Terminfo.PrettyPrint
import Text.PrettyPrint.Free hiding (column)
import Text.Trifecta.Rope.Bytes
import Text.Trifecta.Rope.Delta
import qualified Data.ByteString.UTF8 as UTF8 

outOfRangeEffects :: [ScopedEffect] -> [ScopedEffect]
outOfRangeEffects xs = soft Bold : xs

type Lines = Array (Int,Int) ([ScopedEffect], Char)

(///) :: Ix i => Array i e -> [(i, e)] -> Array i e
a /// xs = a // P.filter (inRange (bounds a) . fst) xs


grow :: Int -> Lines -> Lines
grow y a 
  | inRange (t,b) y = a
  | otherwise = array new [ (i, if inRange old i then a ! i else ([],' ')) | i <- range new ]
  where old@((t,lo),(b,hi)) = bounds a
        new = ((min t y,lo),(max b y,hi))

draw :: [ScopedEffect] -> Int -> Int -> String -> Lines -> Lines
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

data Rendering = Rendering
  { renderingDelta    :: !Delta                  -- focus, the render will keep this visible
  , renderingLineLen  :: {-# UNPACK #-} !Int     -- actual line length
  , renderingLine     :: Lines -> Lines
  , renderingOverlays :: Delta -> Lines -> Lines
  }

instance Show Rendering where
  showsPrec d (Rendering p ll _ _) = showParen (d > 10) $ 
    showString "Rendering " . showsPrec 11 p . showChar ' ' . showsPrec 11 ll . showString " ... ..."

nullRendering :: Rendering -> Bool
nullRendering (Rendering (Columns 0 0) 0 _ _) = True
nullRendering _ = False

emptyRendering :: Rendering
emptyRendering = rendering (Columns 0 0) ""

instance Semigroup Rendering where
  -- an unprincipled hack
  Rendering (Columns 0 0) 0 _ f <> Rendering del len doc g = Rendering del len doc $ \d l -> f d (g d l)
  Rendering del len doc f <> Rendering _ _ _ g = Rendering del len doc $ \d l -> f d (g d l)

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
  source :: t -> (Int, Lines -> Lines) {- return whether to append EOF, the number of bytes, and the the line -}

instance Source String where
  source s 
    | Prelude.elem '\n' s = ( ls, draw [] 0 0 s') 
    | otherwise           = ( ls + Prelude.length end, draw [soft (Foreground Blue), soft Bold] 0 ls end . draw [] 0 0 s') 
    where
      end = "<EOF>" 
      s' = go 0 s
      ls = Prelude.length s' 
      go n ('\t':xs) = let t = 8 - mod n 8 in P.replicate t ' ' ++ go (n + t) xs
      go _ ('\n':_)  = []
      go n (x:xs)    = x : go (n + 1) xs
      go _ []        = []
      

instance Source ByteString where
  source = source . UTF8.toString

-- | create a drawing surface
rendering :: Source s => Delta -> s -> Rendering
rendering del s = case source s of 
  (len, doc) -> Rendering del len doc (\_ l -> l)

(.#) :: (Delta -> Lines -> Lines) -> Rendering -> Rendering
f .# Rendering d ll s g = Rendering d ll s $ \e l -> f e $ g e l 

instance Pretty Rendering where
  pretty r = prettyTerm r >>= const empty

instance PrettyTerm Rendering where
  prettyTerm (Rendering d ll l f) = nesting $ \k -> columns $ \n -> go (n - k) where
    go cols = align (vsep (P.map ln [t..b])) where 
      (lo, hi) = window (column d) ll (min (max (cols - 2) 30) 200)
      a = f d $ l $ array ((0,lo),(-1,hi)) []
      ((t,_),(b,_)) = bounds a
      ln y = hcat 
           $ P.map (\g -> P.foldr with (string (P.map snd g)) (fst (P.head g)))
           $ groupBy ((==) `on` fst) 
           [ a ! (y,i) | i <- [lo..hi] ] 

window :: Int -> Int -> Int -> (Int, Int)
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

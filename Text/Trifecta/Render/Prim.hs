{-# LANGUAGE TypeSynonymInstances #-}
-- | Diagnostics rendering
module Text.Trifecta.Render.Prim 
  ( Render(..)
  , Renderable(..)
  , Source(..)
  , Rendered(..)
  , surface
  , nullRender
  , emptyRender
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
import Data.Monoid
import Data.Function (on)
import Data.Functor.Bind
import Data.List (groupBy)
import Data.Semigroup
import Data.Semigroup.Foldable
import Data.Semigroup.Traversable
import Data.Traversable
import Prelude as P
import Prelude hiding (span)
import System.Console.Terminfo.PrettyPrint
import Text.PrettyPrint.Free hiding (column)
import Text.Trifecta.Bytes
import Text.Trifecta.Delta
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

data Render = Render 
  { rDelta     :: !Delta                  -- focus, the render will keep this visible
  , rLineLen   :: {-# UNPACK #-} !Int     -- actual line length
  , rLine      :: Lines -> Lines
  , rDraw      :: Delta -> Lines -> Lines
  }

instance Show Render where
  showsPrec d (Render p ll _ _) = showParen (d > 10) $ 
    showString "Render " . showsPrec 11 p . showChar ' ' . showsPrec 11 ll . showString " ... ..."

nullRender :: Render -> Bool
nullRender (Render (Columns 0 0) 0 _ _) = True
nullRender _ = False

emptyRender :: Render 
emptyRender = surface (Columns 0 0) ""

instance Semigroup Render where
  -- an unprincipled hack
  Render (Columns 0 0) 0 _ f <> Render del len doc g = Render del len doc $ \d l -> f d (g d l)
  Render del len doc f <> Render _ _ _ g = Render del len doc $ \d l -> f d (g d l)

instance Monoid Render where
  mappend = (<>) 
  mempty = emptyRender
  
ifNear :: Delta -> (Lines -> Lines) -> Delta -> Lines -> Lines
ifNear d f d' l | near d d' = f l 
                | otherwise = l

instance HasDelta Render where
  delta = rDelta

class Renderable t where
  render :: t -> Render 

instance Renderable Render where
  render = id

class Source t where
  source :: t -> (Int, Lines -> Lines)

instance Source String where
  source s = (P.length s', draw [] 0 0 s') where 
    s' = go 0 s
    go n ('\t':xs) = let t = 8 - mod n 8 in P.replicate t ' ' ++ go (n + t) xs
    go _ ('\n':_)  = []
    go n (x:xs)    = x : go (n + 1) xs
    go _ []        = []

instance Source ByteString where
  source = source . UTF8.toString

-- | create a drawing surface
surface :: Source s => Delta -> s -> Render
surface del s = case source s of 
  (len, doc) -> Render del len doc (\_ l -> l)

(.#) :: (Delta -> Lines -> Lines) -> Render -> Render
f .# Render d ll s g = Render d ll s $ \e l -> f e $ g e l 

instance Pretty Render where
  pretty r = prettyTerm r >>= const empty

instance PrettyTerm Render where
  prettyTerm (Render d ll l f) = nesting $ \k -> columns $ \n -> go (n - k) where
    go cols = align (vsep (P.map ln [t..b])) where 
      (lo, hi) = window (column d) ll (cols - 2)
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

data Rendered a = a :@ Render
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

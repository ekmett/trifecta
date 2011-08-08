{-# LANGUAGE TypeSynonymInstances #-}
-- | Diagnostics rendering
module Text.Trifecta.Render 
  ( Render(..)
  , Renderable(..)
  , Source(..)
  , surface
  -- * Lower level drawing primitives
  , Lines
  , draw
  , ifNear
  , (.#)
  ) where

import Data.ByteString hiding (groupBy, empty, any)
import qualified Data.ByteString.UTF8 as UTF8 
import Data.List (groupBy)
import Data.Function (on)
import Text.Trifecta.Delta
import Data.Semigroup
import Data.Array
import Text.PrettyPrint.Free hiding (column)
import System.Console.Terminfo.PrettyPrint
import Control.Monad.State
import Prelude as P

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
draw e y n xs a0 = gt $ lt (a /// out) where 
  a = grow y a0
  ((_,lo),(_,hi)) = bounds a
  out = P.zipWith (\i c -> ((y,i),(e,c))) [n..] xs
  lt | any (\el -> snd (fst el) < lo) out = (// [((y,lo),(outOfRangeEffects e,'<'))])
     | otherwise = id
  gt | any (\el -> snd (fst el) > hi) out = (// [((y,hi),(outOfRangeEffects e,'>'))])
     | otherwise = id

data Render = Render 
  { rDelta     :: !Delta                  -- focus, the render will keep this visible
  , rLineLen   :: {-# UNPACK #-} !Int     -- actual line length
  , rLine      :: Lines -> Lines
  , rDraw      :: Delta -> Lines -> Lines
  }

instance Semigroup Render where
  Render del len doc f <> Render _ _ _ g = Render del len doc $ \d l -> f d (g d l)

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

instance Show Render where
  showsPrec _ = displayS . renderPretty 0.9 100 . prettyTerm

instance PrettyTerm Render where
  prettyTerm (Render d ll l f) = nesting $ \k -> columns $ \n -> go (n - k) where
    go cols = align (vsep (P.map ln [t..b])) <> linebreak where 
      (lo, hi) = window (column d) ll (cols - 2)
      a = f d $ l $ array ((0,lo),(-1,hi)) [] -- blankLine lo hi
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

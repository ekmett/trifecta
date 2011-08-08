{-# LANGUAGE TypeSynonymInstances #-}
-- | Diagnostics rendering
module Text.Trifecta.Render 
  ( Rendering(..)
  , Renderable(..)
  , Source(..)
  , surface
  , draw
  , addSym
  , addFix
  , addCaret
  , addSpan
  , addFixit
  -- * Internals
  , caretEffects, fixitEffects, spanEffects, outOfRangeEffects
  , blankLine
  ) where

import Control.Applicative hiding (empty)
import Data.ByteString hiding (groupBy, empty, any)
import qualified Data.ByteString.UTF8 as UTF8 
import Data.List (groupBy)
import Data.Function (on)
import Text.Trifecta.Delta
import Data.Foldable (toList)
import Data.Array
import Text.Trifecta.Bytes
import Text.PrettyPrint.Leijen.Extras hiding (column)
import System.Console.Terminfo.Color
import System.Console.Terminfo.PrettyPrint
import Control.Monad.State
import Prelude as P

caretEffects, fixitEffects, spanEffects :: [ScopedEffect]
caretEffects = [soft (Foreground Green), soft Bold]
fixitEffects = [soft (Foreground Blue)]
spanEffects  = [soft (Foreground Green)]

outOfRangeEffects :: [ScopedEffect] -> [ScopedEffect]
outOfRangeEffects xs = soft Bold : xs

type Line = Array Int ([ScopedEffect], Char)

(///) :: Ix i => Array i e -> [(i, e)] -> Array i e
a /// xs = a // P.filter (inRange (bounds a) . fst) xs

draw :: [ScopedEffect] -> Int -> String -> Line -> Line
draw e n xs a = gt $ lt $ a /// out
    where (lo,hi) = bounds a
          out = P.zipWith (\i c -> (i,(e,c))) [n..] xs
          lt | any (\el -> fst el < lo) out = (// [(lo,(outOfRangeEffects e,'<'))])
             | otherwise = id
          gt | any (\el -> fst el > hi) out = (// [(hi,(outOfRangeEffects e,'>'))])
             | otherwise = id

data Rendering = Rendering 
  { rDelta   :: !Delta                -- focus, the rendering will keep this visible
  , rLineLen :: {-# UNPACK #-} !Int   -- actual line length
  , rLine    :: Line -> Line          -- source contents
  , rSymbols :: Line -> Line          -- annotations about the line
  , rFixits  :: Maybe (Line -> Line)  -- fixits providing alternate text
  }

addSym, addFix :: Rendering -> (Line -> Line) -> Rendering
addSym r f = r { rSymbols = f . rSymbols r }
addFix r f = r { rFixits = fmap (f .) (rFixits r) <|> Just f } 

instance HasDelta Rendering where
  delta = rDelta

class Renderable t where
  rendering :: t -> Rendering 

instance Renderable Rendering where
  rendering = id

class Source t where
  source :: t -> (Int, Line -> Line)

instance Source String where
  source s = (P.length s', draw [] 0 s') where s' = expand s

instance Source ByteString where
  source = source . UTF8.toString

-- | create a drawing surface
surface :: Source s => Delta -> s -> Rendering
surface d s = case source s of 
  (ls, doc) -> Rendering d ls doc id Nothing

expand :: String -> String
expand = go 0 where
  go n ('\t':xs) = let t = 8 - mod n 8 in P.replicate t ' ' ++ go (n + t) xs
  go _ ('\n':_)  = []
  go n (x:xs)    = x : go (n + 1) xs
  go _ []        = []

addCaret :: Delta -> Rendering -> Rendering 
addCaret p r 
  | near p r  = addSym r $ draw caretEffects (column p) "^"
  | otherwise = r

addSpan :: Delta -> Delta -> Rendering -> Rendering 
addSpan s e r
  | nl && nh  = addSym r $ draw spanEffects (column l) $ P.replicate (max (column h   - column l + 1) 0) '~' 
  | nl        = addSym r $ draw spanEffects (column l) $ P.replicate (max (rLineLen r - column l + 2) 0) '~'
  |       nh  = addSym r $ draw spanEffects (-1)       $ P.replicate (max (column h + 1) 0) '~'
  | otherwise = r
  where 
    l = argmin bytes s e 
    h = argmax bytes s e
    nl = near l r
    nh = near h r

addFixit :: Delta -> Delta -> String -> Rendering -> Rendering
addFixit s e rpl r
  | near l r = addFix r' $ draw fixitEffects (column l) rpl
  | otherwise = r'
  where 
    l = argmin bytes s e
    r' = addSpan s e r

instance Pretty Rendering where
  pretty r = prettyTerm r >>= const empty

instance PrettyTerm Rendering where
  prettyTerm r = nesting $ \k -> columns $ \n -> go (n - k) where
    go cols = align (vsep img) <> linebreak where 
      (lo, hi) = window (column r) (rLineLen r) (cols - 2)
      line1 = cluster $ rLine r
      line2 = cluster $ rSymbols r 
      img = case cluster <$> rFixits r of 
        Just line3 -> [line1, line2, line3] 
        Nothing    -> [line1, line2]
      cluster :: (Line -> Line) -> TermDoc
      cluster m = hcat 
                . P.map (\g -> P.foldr with (string (P.map snd g)) (fst (P.head g)))
                . groupBy ((==) `on` fst)
                . toList 
                $ m (blankLine lo hi)

blankLine :: Int -> Int -> Line
blankLine lo hi = listArray (lo,hi) (repeat ([],' '))

window :: Int -> Int -> Int -> (Int, Int)
window c l w 
  | c <= w2     = (0, min w l)
  | c + w2 >= l = if l > w then (l-w, l) else (0, w)
  | otherwise   = (c-w2,c + w2)
  where w2 = div w 2

argmin :: Ord b => (a -> b) -> a -> a -> a
argmin f a b 
  | f a <= f b = a
  | otherwise = b

argmax :: Ord b => (a -> b) -> a -> a -> a
argmax f a b 
  | f a > f b = a
  | otherwise = b

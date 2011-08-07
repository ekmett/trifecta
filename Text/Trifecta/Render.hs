module Text.Trifecta.Render
  ( Rendering(..)
  , rendering
  , render
  , addSymbol
  , addFixit
  , effect
  , drawCover
  , drawCaret
  ) where

import Data.Ix (inRange)
import Data.ByteString hiding (groupBy)
import qualified Data.ByteString.UTF8 as UTF8 
import Data.List (groupBy)
import Data.Function (on)
import Data.IntMap as IM
import Text.Trifecta.Delta
import Text.Trifecta.Bytes
import Text.Trifecta.Caret
import Text.PrettyPrint.Leijen.Extras hiding (column)
import Control.Monad.State
import Prelude as P

type Effect e = Doc e -> Doc e
type EffectId = Int

data Rendering e = Rendering 
  { rDelta   :: !Delta
  , rLine    :: String
  , rFresh   :: !EffectId
  , rEffects :: !(IntMap (Effect e))
  , rSymbols :: !(IntMap (EffectId, Char))
  , rFixits  :: !(IntMap (EffectId, Char))
  }

instance HasDelta (Rendering e) where
  delta = rDelta

rendering :: (Doc e -> Doc e) -> Delta -> ByteString -> Rendering e
rendering bold d bs = Rendering d (expand bs) 2 (IM.fromList [(0,id),(1,bold)]) IM.empty IM.empty where
  expand :: ByteString -> String
  expand = go 0 . UTF8.toString where
    go n ('\t':xs) = let t = 8 - mod n 8 in P.replicate t ' ' ++ go (n + t) xs
    go _ ('\n':_)  = []
    go n (x:xs)    = x : go (n + 1) xs
    go _ []        = []

effect :: Effect e -> State (Rendering e) EffectId
effect f = do
   s <- get
   let eff = rFresh s
   put s { rFresh = eff + 1, rEffects = IM.insert eff f (rEffects s) }
   return eff

drawCaret :: EffectId -> Caret -> Rendering e -> Rendering e
drawCaret eff (Caret p _) r 
  | near p r  = addSymbol (column p) eff "^" r
  | otherwise = r

drawCover :: EffectId -> Cover -> Rendering e -> Rendering e
drawCover eff (Cover (Caret s _) e) r
  | nl && nh  = addSymbol (column l) eff (P.replicate (column h - column l + 1) '~') r
  | nl        = addSymbol (column l) eff (P.replicate (cols     - column l) '~' ++ ">") r
  | nh        = addSymbol 0 eff ('<' : P.replicate (column l) '~') r
  | otherwise = r
  where 
    l = argmin bytes s e 
    h = argmax bytes s e
    nl = near l r
    nh = near h r
    cols = P.length (rLine r)

addSymbol, addFixit :: Int -> EffectId -> String -> Rendering e -> Rendering e
addSymbol n eff xs0 r = r { rSymbols = interval n eff xs0 (rSymbols r) }
addFixit n eff xs0 r = r { rSymbols = interval n eff xs0 (rSymbols r) }

render :: Rendering e -> Doc e
render r = nesting $ \k -> columns $ \n -> go (n - k) where
  go cols = (dots $ align $ vsep img) <> linebreak
    where (dots, rdots, lo, hi) = window (cols - 7) r
          -- line1, line2, line3 :: Doc e
          line1 = rdots $ string $ P.take (hi - lo + 1) $ P.drop lo $ rLine r
          line2 = cluster rSymbols
          line3 = cluster rFixits
          hasFixits = P.any (inRange (lo, hi)) $ IM.keys (rFixits r)
          img | hasFixits = [line1, line2, line3] 
              | otherwise = [line1, line2]
          cluster m = hcat 
                    . P.map (\g -> findWithDefault id (fst (P.head g)) (rEffects r) $ string (P.map snd g))
                    . groupBy ((==) `on` fst)
                    $ P.map (\i -> findWithDefault (0,' ') i (m r)) [lo .. hi]

window :: Int -> Rendering e -> (Doc e -> Doc e, Doc e -> Doc e, Int, Int)
window w r 
  | clamp_lo  && clamp_hi = (id,        id,        0,    w     )
  | clamp_lo              = (id,        (<> dots), 0,    w     )
  |              clamp_hi = ((dots <>), id       , l-w,  l     )
  | otherwise             = ((dots <>), (<> dots), c-w2, c + w2)
  where 
    bold = rEffects r IM.! 1
    dots = bold $ text "..."
    l = P.length $ rLine r
    w2 = div w 2
    c = column r
    clamp_lo = c <= w2
    clamp_hi = c + w2 > l

interval :: Int -> EffectId -> String -> IntMap (EffectId, Char) -> IntMap (EffectId, Char)
interval _ _   []     = id
interval k eff (x:xs) = interval (k + 1) eff xs . insert k (eff,x)

argmin :: Ord b => (a -> b) -> a -> a -> a
argmin f a b | f a <= f b = a
             | otherwise  = b

argmax :: Ord b => (a -> b) -> a -> a -> a
argmax f a b | f a > f b = a
             | otherwise = b


{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeSynonymInstances  #-}
-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2011-2015 Edward Kmett
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

import           Control.Applicative
import           Control.Comonad
import           Control.Lens
import           Data.Array
import           Data.ByteString              as B hiding (any, empty, groupBy)
import qualified Data.ByteString.UTF8         as UTF8
import           Data.Data
import           Data.Foldable
import           Data.Function                (on)
import           Data.Hashable
import           Data.Int                     (Int64)
import           Data.List                    (groupBy)
import           Data.Maybe
import           Data.Semigroup
import           Data.Semigroup.Reducer
import           GHC.Generics
import           Prelude                      as P hiding (span)
import           System.Console.ANSI
import           Text.PrettyPrint.ANSI.Leijen hiding (column, (<$>), (<>))

import Text.Trifecta.Delta
import Text.Trifecta.Instances        ()
import Text.Trifecta.Util.Combinators

-- $setup
--
-- >>> :set -XOverloadedStrings
-- >>> let exampleRendering = rendered mempty ("int main(int argc, char ** argv) { int; }" :: ByteString)

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

-- | A raw canvas to paint ANSI-styled characters on.
type Lines = Array (Int,Int64) ([SGR], Char)

-- | Remove a number of @(index, element)@ values from an @'Array'@.
(///) :: Ix i => Array i e -> [(i, e)] -> Array i e
a /// xs = a // P.filter (inRange (bounds a) . fst) xs

grow :: Int -> Lines -> Lines
grow y a
  | inRange (t,b) y = a
  | otherwise = array new [ (i, if inRange old i then a ! i else ([],' ')) | i <- range new ]
  where old@((t,lo),(b,hi)) = bounds a
        new = ((min t y,lo),(max b y,hi))

draw
    :: [SGR]  -- ^ ANSI style to use
    -> Int    -- ^ Line; 0 is at the top
    -> Int64  -- ^ Column; 0 is on the left
    -> String -- ^ Data to be written
    -> Lines  -- ^ Canvas to draw on
    -> Lines
draw _ _ _ "" a0 = a0
draw e y n xs a0 = gt $ lt (a /// out)
  where
    a = grow y a0
    ((_,lo),(_,hi)) = bounds a
    out = P.zipWith (\i c -> ((y,i),(e,c))) [n..] xs
    lt | P.any (\el -> snd (fst el) < lo) out = (// [((y,lo),(outOfRangeEffects e,'<'))])
       | otherwise = id
    gt | P.any (\el -> snd (fst el) > hi) out = (// [((y,hi),(outOfRangeEffects e,'>'))])
       | otherwise = id

-- | A 'Rendering' is a canvas of text that output can be written to.
data Rendering = Rendering
  { _renderingDelta :: !Delta
    -- ^ focus, the render will keep this visible

  , _renderingLineLen :: {-# UNPACK #-} !Int64
    -- ^ actual line length

  , _renderingLineBytes :: {-# UNPACK #-} !Int64
    -- ^ line length in bytes

  , _renderingLine :: Lines -> Lines

  , _renderingOverlays :: Delta -> Lines -> Lines
  }

makeClassy ''Rendering

instance Show Rendering where
  showsPrec d (Rendering p ll lb _ _) = showParen (d > 10) $
    showString "Rendering " . showsPrec 11 p . showChar ' ' . showsPrec 11 ll . showChar ' ' . showsPrec 11 lb . showString " ... ..."

-- | Is the 'Rendering' empty?
--
-- >>> nullRendering emptyRendering
-- True
--
-- >>> nullRendering exampleRendering
-- False
nullRendering :: Rendering -> Bool
nullRendering (Rendering (Columns 0 0) 0 0 _ _) = True
nullRendering _ = False

-- | The empty 'Rendering', which contains nothing at all.
--
-- >>> show (pretty emptyRendering)
-- ""
emptyRendering :: Rendering
emptyRendering = Rendering (Columns 0 0) 0 0 id (const id)

instance Semigroup Rendering where
  -- an unprincipled hack
  Rendering (Columns 0 0) 0 0 _ f <> Rendering del len lb dc g = Rendering del len lb dc $ \d l -> f d (g d l)
  Rendering del len lb dc f <> Rendering _ _ _ _ g = Rendering del len lb dc $ \d l -> f d (g d l)

instance Monoid Rendering where
  mappend = (<>)
  mempty = emptyRendering

ifNear
    :: Delta            -- ^ Position 1
    -> (Lines -> Lines) -- ^ Modify the fallback result if the positions are 'near' each other
    -> Delta            -- ^ Position 2
    -> Lines            -- ^ Fallback result if the positions are not 'near' each other
    -> Lines
ifNear d f d' l | near d d' = f l
                | otherwise = l

instance HasDelta Rendering where
  delta = _renderingDelta

class Renderable t where
  render :: t -> Rendering

instance Renderable Rendering where
  render = id

class Source t where
  source :: t -> (Int64, Int64, Lines -> Lines)
  -- ^ @
  -- ( Number of (padded) columns
  -- , number of bytes
  -- , line )
  -- @

instance Source String where
  source s
    | P.elem '\n' s = (ls, bs, draw [] 0 0 s')
    | otherwise           = ( ls + fromIntegral (P.length end), bs, draw [SetColor Foreground Vivid Blue, SetConsoleIntensity BoldIntensity] 0 ls end . draw [] 0 0 s')
    where
      end = "<EOF>"
      s' = go 0 s
      bs = fromIntegral $ B.length $ UTF8.fromString $ P.takeWhile (/='\n') s
      ls = fromIntegral $ P.length s'
      go n ('\t':xs) = let t = 8 - mod n 8 in P.replicate t ' ' ++ go (n + t) xs
      go _ ('\n':_)  = []
      go n (x:xs)    = x : go (n + 1) xs
      go _ []        = []

instance Source ByteString where
  source = source . UTF8.toString

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
  | c + w2 >= l = if l > w then (l-w, l)
                           else (0  , w)
  | otherwise   = (c-w2, c+w2)
  where w2 = div w 2

data Rendered a = a :@ Rendering
  deriving Show

instance Functor Rendered where
  fmap f (a :@ s) = f a :@ s

instance HasDelta (Rendered a) where
  delta = delta . render

instance HasBytes (Rendered a) where
  bytes = bytes . delta

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

-- | A 'Caret' marks a point in the input with a simple @^@ character.
--
-- >>> plain (pretty (addCaret (Columns 35 35) exampleRendering))
-- int main(int argc, char ** argv) { int; }<EOF>
--                                    ^
data Caret = Caret !Delta {-# UNPACK #-} !ByteString deriving (Eq,Ord,Show,Data,Typeable,Generic)

class HasCaret t where
  caret :: Lens' t Caret

instance HasCaret Caret where
  caret = id

instance Hashable Caret

-- | ANSI terminal style for rendering the caret.
caretEffects :: [SGR]
caretEffects = [SetColor Foreground Vivid Green]

drawCaret :: Delta -> Delta -> Lines -> Lines
drawCaret p = ifNear p $ draw caretEffects 1 (fromIntegral (column p)) "^"

-- | Render a caret at a certain position in a 'Rendering'.
addCaret :: Delta -> Rendering -> Rendering
addCaret p r = drawCaret p .# r

instance HasBytes Caret where
  bytes = bytes . delta

instance HasDelta Caret where
  delta (Caret d _) = d

instance Renderable Caret where
  render (Caret d bs) = addCaret d $ rendered d bs

instance Reducer Caret Rendering where
  unit = render

instance Semigroup Caret where
  a <> _ = a

renderingCaret :: Delta -> ByteString -> Rendering
renderingCaret d bs = addCaret d $ rendered d bs

data Careted a = a :^ Caret deriving (Eq,Ord,Show,Data,Typeable,Generic)

instance HasCaret (Careted a) where
  caret f (a :^ c) = (a :^) <$> f c

instance Functor Careted where
  fmap f (a :^ s) = f a :^ s

instance HasDelta (Careted a) where
  delta (_ :^ c) = delta c

instance HasBytes (Careted a) where
  bytes (_ :^ c) = bytes c

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

-- | ANSI terminal style to render spans with.
spanEffects :: [SGR]
spanEffects  = [SetColor Foreground Dull Green]

drawSpan
    :: Delta -- ^ Start of the region of interest
    -> Delta -- ^ End of the region of interest
    -> Delta -- ^ Currrent location
    -> Lines -- ^ 'Lines' to add the rendering to
    -> Lines
drawSpan start end d a
  | nearLo && nearHi = go (column lo) (rep (max (column hi - column lo) 0) '~') a
  | nearLo           = go (column lo) (rep (max (snd (snd (bounds a)) - column lo + 1) 0) '~') a
  |           nearHi = go (-1)        (rep (max (column hi + 1) 0) '~') a
  | otherwise        = a
  where
    go = draw spanEffects 1 . fromIntegral
    lo = argmin bytes start end
    hi = argmax bytes start end
    nearLo = near lo d
    nearHi = near hi d
    rep = P.replicate . fromIntegral

addSpan :: Delta -> Delta -> Rendering -> Rendering
addSpan s e r = drawSpan s e .# r

-- | A 'Span' marks a range of input characters. If 'Caret' is a point, then
-- 'Span' is a line.
--
-- >>> plain (pretty (addSpan (Columns 35 35) (Columns 38 38) exampleRendering))
-- int main(int argc, char ** argv) { int; }<EOF>
--                                    ~~~
data Span = Span !Delta !Delta {-# UNPACK #-} !ByteString deriving (Eq,Ord,Show,Data,Typeable,Generic)

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

-- | Annotate an arbitrary piece of data with a 'Span', typically its
-- corresponding input location.
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

drawFixit :: Delta -> Delta -> String -> Delta -> Lines -> Lines
drawFixit s e rpl d a = ifNear l (draw [SetColor Foreground Dull Blue] 2 (fromIntegral (column l)) rpl) d
                      $ drawSpan s e d a
  where l = argmin bytes s e

addFixit :: Delta -> Delta -> String -> Rendering -> Rendering
addFixit s e rpl r = drawFixit s e rpl .# r

-- | A 'Fixit' is a 'Span' with a suggestion.
--
-- >>> plain (pretty (addFixit (Columns 35 35) (Columns 38 38) "Fix this!" exampleRendering))
-- int main(int argc, char ** argv) { int; }<EOF>
--                                    ~~~
--                                    Fix this!
data Fixit = Fixit
  { _fixitSpan :: {-# UNPACK #-} !Span
    -- ^ 'Span' where the error occurred
  , _fixitReplacement :: !ByteString
    -- ^ Replacement suggestion
  } deriving (Eq,Ord,Show,Data,Typeable,Generic)

makeClassy ''Fixit

instance HasSpan Fixit where
  span = fixitSpan

instance Hashable Fixit

instance Reducer Fixit Rendering where
  unit = render

instance Renderable Fixit where
  render (Fixit (Span s e bs) r) = addFixit s e (UTF8.toString r) $ rendered s bs

{-# language CPP                   #-}
{-# language DeriveDataTypeable    #-}
{-# language DeriveGeneric         #-}
{-# language FlexibleInstances     #-}
{-# language MultiParamTypeClasses #-}
{-# language TemplateHaskell       #-}
{-# language TypeSynonymInstances  #-}
-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2011-2019 Edward Kmett
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
  , prettyRendering
  , Source(..)
  , rendered
  , Renderable(..)
  , Rendered(..)
  , gutterEffects
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
import           Data.ByteString                              as B hiding (any, empty, groupBy)
import qualified Data.ByteString.UTF8                         as UTF8
import           Data.Data
import           Data.Foldable
import           Data.Function                                (on)
import           Data.Hashable
import           Data.Int                                     (Int64)
import qualified Data.List.NonEmpty                           as NE
import           Data.Maybe
import           Data.Semigroup
import           Data.Semigroup.Reducer
import           GHC.Generics
import           Prelude                                      as P hiding (span)
import           Prettyprinter                                hiding (column, line')
import           Prettyprinter.Render.Terminal                (color, bgColor, colorDull, bgColorDull)
import qualified Prettyprinter.Render.Terminal                as Pretty
import           System.Console.ANSI

import Text.Trifecta.Delta
import Text.Trifecta.Util.Combinators
import Text.Trifecta.Util.Pretty

-- $setup
--
-- >>> :set -XOverloadedStrings
-- >>> import Data.ByteString (ByteString)
-- >>> import Data.Monoid (mempty)
-- >>> import Prettyprinter (unAnnotate)
-- >>> import Text.Trifecta.Delta
-- >>> let exampleRendering = rendered mempty ("int main(int argc, char ** argv) { int; }" :: ByteString)

outOfRangeEffects :: [SGR] -> [SGR]
outOfRangeEffects xs = SetConsoleIntensity BoldIntensity : xs

sgr :: [SGR] -> Doc AnsiStyle -> Doc AnsiStyle
sgr xs0 = go (P.reverse xs0) where
  go []                                         = id
  go (SetConsoleIntensity NormalIntensity : xs) = annotate debold . go xs
  go (SetConsoleIntensity BoldIntensity   : xs) = annotate bold . go xs
  go (SetUnderlining NoUnderline          : xs) = annotate deunderline . go xs
  go (SetUnderlining SingleUnderline      : xs) = annotate underlined . go xs
  go (SetColor f i c                      : xs) = case f of
#if MIN_VERSION_ansi_terminal(1,1,0)
    Underlining -> go xs
#endif
    Foreground -> case i of
      Dull -> case c of
        Black   -> annotate (color Pretty.Black) . go xs
        Red     -> annotate (color Pretty.Red) . go xs
        Green   -> annotate (color Pretty.Green) . go xs
        Yellow  -> annotate (color Pretty.Yellow) . go xs
        Blue    -> annotate (color Pretty.Blue) . go xs
        Magenta -> annotate (color Pretty.Magenta) . go xs
        Cyan    -> annotate (color Pretty.Cyan) . go xs
        White   -> annotate (color Pretty.White) . go xs
      Vivid -> case c of
        Black   -> annotate (colorDull Pretty.Black) . go xs
        Red     -> annotate (colorDull Pretty.Red) . go xs
        Green   -> annotate (colorDull Pretty.Green) . go xs
        Yellow  -> annotate (colorDull Pretty.Yellow) . go xs
        Blue    -> annotate (colorDull Pretty.Blue) . go xs
        Magenta -> annotate (colorDull Pretty.Magenta) . go xs
        Cyan    -> annotate (colorDull Pretty.Cyan) . go xs
        White   -> annotate (colorDull Pretty.White) . go xs
    Background -> case i of
      Dull -> case c of
        Black   -> annotate (bgColorDull Pretty.Black) . go xs
        Red     -> annotate (bgColorDull Pretty.Red) . go xs
        Green   -> annotate (bgColorDull Pretty.Green) . go xs
        Yellow  -> annotate (bgColorDull Pretty.Yellow) . go xs
        Blue    -> annotate (bgColorDull Pretty.Blue) . go xs
        Magenta -> annotate (bgColorDull Pretty.Magenta) . go xs
        Cyan    -> annotate (bgColorDull Pretty.Cyan) . go xs
        White   -> annotate (bgColorDull Pretty.White) . go xs
      Vivid -> case c of
        Black   -> annotate (bgColor Pretty.Black) . go xs
        Red     -> annotate (bgColor Pretty.Red) . go xs
        Green   -> annotate (bgColor Pretty.Green) . go xs
        Yellow  -> annotate (bgColor Pretty.Yellow) . go xs
        Blue    -> annotate (bgColor Pretty.Blue) . go xs
        Magenta -> annotate (bgColor Pretty.Magenta) . go xs
        Cyan    -> annotate (bgColor Pretty.Cyan) . go xs
        White   -> annotate (bgColor Pretty.White) . go xs
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
-- >>> show (prettyRendering emptyRendering)
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

prettyRendering :: Rendering -> Doc AnsiStyle
prettyRendering (Rendering d ll _ l f) = nesting $ \k -> columns $ \mn -> go (fromIntegral (fromMaybe 80 mn - k)) where
  go cols = align (vsep (P.map ln [t..b])) where
    (lo, hi) = window (column d) ll (min (max (cols - 5 - fromIntegral gutterWidth) 30) 200)
    a = f d $ l $ array ((0,lo),(-1,hi)) []
    ((t,_),(b,_)) = bounds a
    n = show $ case d of
      Lines      n' _ _ _ -> 1 + n'
      Directed _ n' _ _ _ -> 1 + n'
      _                   -> 1
    separator = char '|'
    gutterWidth = P.length n
    gutter = pretty n <+> separator
    margin = fill gutterWidth space <+> separator
    ln y = (sgr gutterEffects (if y == 0 then gutter else margin) <+>)
         $ hcat
         $ P.map (\g -> sgr (fst (NE.head g)) (pretty (fmap snd g)))
         $ NE.groupBy ((==) `on` fst)
         [ a ! (y,i) | i <- [lo..hi] ]

window :: Int64 -> Int64 -> Int64 -> (Int64, Int64)
window c l w
  | c <= w2     = (0, min w l)
  | c + w2 >= l = if l > w then (l-w, l)
                           else (0  , w)
  | otherwise   = (c-w2, c+w2)
  where w2 = div w 2

-- | ANSI terminal style for rendering the gutter.
gutterEffects :: [SGR]
gutterEffects = [SetColor Foreground Vivid Blue]

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
-- >>> unAnnotate (prettyRendering (addCaret (Columns 35 35) exampleRendering))
-- 1 | int main(int argc, char ** argv) { int; }<EOF>
--   |                                    ^
data Caret = Caret !Delta {-# UNPACK #-} !ByteString deriving (Eq,Ord,Show,Data,Generic)

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

data Careted a = a :^ Caret deriving (Eq,Ord,Show,Data,Generic)

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
-- >>> unAnnotate (prettyRendering (addSpan (Columns 35 35) (Columns 38 38) exampleRendering))
-- 1 | int main(int argc, char ** argv) { int; }<EOF>
--   |                                    ~~~
data Span = Span !Delta !Delta {-# UNPACK #-} !ByteString deriving (Eq,Ord,Show,Data,Generic)

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
data Spanned a = a :~ Span deriving (Eq,Ord,Show,Data,Generic)

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
-- >>> unAnnotate (prettyRendering (addFixit (Columns 35 35) (Columns 38 38) "Fix this!" exampleRendering))
-- 1 | int main(int argc, char ** argv) { int; }<EOF>
--   |                                    ~~~
--   |                                    Fix this!
data Fixit = Fixit
  { _fixitSpan :: {-# UNPACK #-} !Span
    -- ^ 'Span' where the error occurred
  , _fixitReplacement :: !ByteString
    -- ^ Replacement suggestion
  } deriving (Eq,Ord,Show,Data,Generic)

makeClassy ''Fixit

instance HasSpan Fixit where
  span = fixitSpan

instance Hashable Fixit

instance Reducer Fixit Rendering where
  unit = render

instance Renderable Fixit where
  render (Fixit (Span s e bs) r) = addFixit s e (UTF8.toString r) $ rendered s bs

{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Trifecta.Rope.Highlighted
-- Copyright   :  (C) 2011 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
----------------------------------------------------------------------------
module Text.Trifecta.Rope.Highlighted
  ( HighlightedRope(..)
  ) where

import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.ByteString.Lazy.UTF8 as LazyUTF8
import Data.Foldable as F
import Data.Int (Int64)
import Text.Trifecta.IntervalMap as IM
import Data.Key hiding ((!))
import Data.List (sort)
import Data.Semigroup
import Data.Semigroup.Union
import Prelude hiding (head)
import System.Console.Terminfo.PrettyPrint
import Text.Blaze
import Text.Blaze.Internal
import Text.Blaze.Html5 hiding (b,i)
import Text.Blaze.Html5.Attributes hiding (title)
import Text.Trifecta.Rope.Delta
import Text.Trifecta.Rope.Bytes
import Text.Trifecta.Rope.Prim
import Text.Trifecta.Highlight.Class
import Text.Trifecta.Highlight.Effects
import Text.Trifecta.Highlight.Prim
import Text.PrettyPrint.Free

data HighlightedRope = HighlightedRope 
  { ropeHighlights :: !Highlights
  , ropeContent    :: {-# UNPACK #-} !Rope 
  }

instance HasDelta HighlightedRope where
  delta = delta . ropeContent

instance HasBytes HighlightedRope where
  bytes = bytes . ropeContent

instance Semigroup HighlightedRope where
  HighlightedRope h bs <> HighlightedRope h' bs' = HighlightedRope (h `union` IM.offset (delta bs) h') (bs <> bs')

instance Monoid HighlightedRope where
  mappend = (<>) 
  mempty = HighlightedRope mempty mempty

instance Highlightable HighlightedRope where
  addHighlights h (HighlightedRope h' r) = HighlightedRope (h `union` h') r

data Located a = a :@ {-# UNPACK #-} !Int64
infix 5 :@
instance Eq (Located a) where
  _ :@ m == _ :@ n = m == n
instance Ord (Located a) where
  compare (_ :@ m) (_ :@ n) = compare m n

instance ToHtml HighlightedRope where
  toHtml (HighlightedRope intervals r) = pre $ go 0 lbs effects where 
    lbs = L.fromChunks [bs | Strand bs _ <- F.toList (strands r)]
    ln no = a ! name (toValue $ "line-" ++ show no) $ Empty
    effects = sort $ [ i | (Interval lo hi, tok) <- intersections mempty (delta r) intervals
                     , i <- [ (Leaf "span" "<span" ">" ! class_ (toValue $ show tok)) :@ bytes lo
                            , preEscapedString "</span>" :@ bytes hi
                            ]
                     ] ++ mapWithKey (\k i -> ln k :@ i) (L.elemIndices '\n' lbs)
    go _ cs [] = unsafeLazyByteString cs
    go b cs ((eff :@ eb) : es) 
      | eb <= b = eff >> go b cs es 
      | otherwise = unsafeLazyByteString om >> go eb nom es
         where (om,nom) = L.splitAt (fromIntegral (eb - b)) cs

instance Pretty HighlightedRope where
  pretty (HighlightedRope _ r) = hsep $ [ pretty bs | Strand bs _ <- F.toList (strands r)]

instance PrettyTerm HighlightedRope where
  prettyTerm (HighlightedRope intervals r) = go 0 lbs effects where
    lbs = L.fromChunks [bs | Strand bs _ <- F.toList (strands r)]
    effects = sort $ [ i | (Interval lo hi, tok) <- intersections mempty (delta r) intervals
                     , i <- [ pushToken tok :@ bytes lo
                            , popToken tok  :@ bytes hi
                            ]
                     ]
    go _ cs [] = prettyTerm (LazyUTF8.toString cs)
    go b cs ((eff :@ eb) : es) 
      | eb <= b = eff <> go b cs es 
      | otherwise = prettyTerm (LazyUTF8.toString om) <> go eb nom es
         where (om,nom) = L.splitAt (fromIntegral (eb - b)) cs

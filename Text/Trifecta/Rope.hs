-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Trifecta.Rope
-- Copyright   :  (C) 2011 Edward Kmett,
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
----------------------------------------------------------------------------
module Text.Trifecta.Rope 
  ( Rope, rope, strands
  -- * Strands of a rope
  , Strand(..), strand
  -- * Properties
  , Delta(..)
  , HasDelta(..)
  , HasBytes(..)
  , HighlightedRope(..)
  ) where

import Text.Trifecta.Rope.Prim
import Text.Trifecta.Rope.Delta
import Text.Trifecta.Rope.Bytes
import Text.Trifecta.Rope.Highlighted

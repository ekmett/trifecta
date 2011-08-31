-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Trifecta.Highlight.Prim
-- Copyright   :  (C) 2011 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
----------------------------------------------------------------------------
module Text.Trifecta.Highlight.Prim
  ( Highlight(..)
  , Highlights
  ) where

import Data.Ix
import Text.Trifecta.IntervalMap
import Text.Trifecta.Rope.Delta

data Highlight
  = EscapeCode
  | Number 
  | Comment
  | CharLiteral
  | StringLiteral
  | Constant
  | Statement
  | Special
  | Symbol
  | Identifier
  | ReservedIdentifier
  | Operator
  | ReservedOperator
  | Constructor
  | ReservedConstructor
  | ConstructorOperator
  | ReservedConstructorOperator
  deriving (Eq,Ord,Show,Read,Enum,Ix,Bounded)

type Highlights = IntervalMap Delta Highlight

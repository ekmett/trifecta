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
  deriving (Eq,Ord,Show,Read,Enum,Ix,Bounded)

type Highlights = IntervalMap Delta Highlight



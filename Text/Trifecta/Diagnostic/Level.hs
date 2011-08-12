module Text.Trifecta.Diagnostic.Level 
  ( DiagnosticLevel(..)
  ) where

import Control.Applicative
import Data.Semigroup
import Text.PrettyPrint.Free
import System.Console.Terminfo.PrettyPrint

data DiagnosticLevel = Verbose !Int | Note | Warning | Error | Fatal
  deriving (Eq,Show,Read)

instance Ord DiagnosticLevel where
  compare (Verbose n) (Verbose m) = compare m n
  compare (Verbose _) _ = LT
  compare Note (Verbose _) = GT
  compare Note Note = EQ
  compare Note _ = LT
  compare Warning (Verbose _) = GT
  compare Warning Note = GT
  compare Warning Warning = EQ
  compare Warning _ = LT
  compare Error Error = EQ
  compare Error Fatal = LT
  compare Error _     = GT
  compare Fatal Fatal = EQ
  compare Fatal _     = GT

instance Semigroup DiagnosticLevel where
  (<>) = max

instance Pretty DiagnosticLevel where
  pretty p = prettyTerm p *> empty

instance PrettyTerm DiagnosticLevel where
  prettyTerm (Verbose n) = blue $ text "verbose (" <> int n <> char ')'
  prettyTerm Note        = black $ text "note"
  prettyTerm Warning     = magenta $ text "warning"
  prettyTerm Error       = red $ text "error"
  prettyTerm Fatal       = standout $ red $ text "fatal"

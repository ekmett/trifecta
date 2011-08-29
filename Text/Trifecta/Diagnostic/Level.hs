module Text.Trifecta.Diagnostic.Level 
  ( DiagnosticLevel(..)
  ) where

import Control.Applicative
import Data.Semigroup
import Text.PrettyPrint.Free
import System.Console.Terminfo.PrettyPrint

data DiagnosticLevel 
  = Verbose !Int -- a comment we should only show to the excessively curious
  | Note         -- a comment
  | Warning      -- a warning, computation continues
  | Error        -- a user specified error
  | Fatal        -- a user specified fatal error 
  | Panic        -- a non-maskable death sentence thrown by the parser itself
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
  compare Error Panic = LT
  compare Error _     = GT
  compare Fatal Panic = LT
  compare Fatal Fatal = EQ
  compare Fatal _     = GT
  compare Panic Panic = EQ
  compare Panic _     = GT

instance Semigroup DiagnosticLevel where
  (<>) = max

instance Pretty DiagnosticLevel where
  pretty p = prettyTerm p *> empty

instance PrettyTerm DiagnosticLevel where
  prettyTerm (Verbose n) = blue    $ text "verbose (" <> prettyTerm n <> char ')'
  prettyTerm Note        = black   $ text "note"
  prettyTerm Warning     = magenta $ text "warning"
  prettyTerm Error       = red            $ text "error"
  prettyTerm Fatal       = standout $ red $ text "fatal"
  prettyTerm Panic       = standout $ red $ text "panic"

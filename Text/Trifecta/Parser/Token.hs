-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Trifecta.Parser.Token
-- Copyright   :  (C) 2011 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
----------------------------------------------------------------------------
module Text.Trifecta.Parser.Token
  ( module Text.Trifecta.Parser.Token.Class
  , module Text.Trifecta.Parser.Token.Combinators
  , module Text.Trifecta.Parser.Token.Identifier
  -- * Text.Trifecta.Parser.Prim
  , decimal
  , hexadecimal
  , octal
  ) where

import Text.Trifecta.Parser.Token.Class
import Text.Trifecta.Parser.Token.Prim
import Text.Trifecta.Parser.Token.Combinators
import Text.Trifecta.Parser.Token.Identifier

-- expected to be imported manually
-- import Text.Trifecta.Parser.Token.Style
-- import Text.Trifecta.Parser.Token.Identifier.Style

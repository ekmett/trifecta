-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2011-2019 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- For a short introduction, see the "Text.Trifecta.Tutorial" module.
----------------------------------------------------------------------------
module Text.Trifecta
  ( module Text.Trifecta.Rendering
  , module Text.Trifecta.Highlight
  , module Text.Trifecta.Parser
  , module Text.Trifecta.Combinators
  , module Text.Trifecta.Result
  , module Text.Trifecta.Rope
  , module Text.Parser.Combinators
  , module Text.Parser.Char
  , module Text.Parser.Token
  ) where

import Text.Parser.Char
import Text.Parser.Combinators
import Text.Parser.Token
import Text.Trifecta.Combinators
import Text.Trifecta.Highlight
import Text.Trifecta.Parser
import Text.Trifecta.Rendering
import Text.Trifecta.Result
import Text.Trifecta.Rope

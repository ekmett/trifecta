{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Trifecta.Instances
-- Copyright   :  (c) Edward Kmett 2013
-- License     :  BSD3
--
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Orphan instances we need to remain sane.
-----------------------------------------------------------------------------
module Text.Trifecta.Instances () where

import Text.PrettyPrint.ANSI.Leijen
import qualified Data.Semigroup as Data

instance Data.Semigroup Doc where
  (<>) = (<>)

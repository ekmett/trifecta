{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Edward Kmett 2013-2015
-- License     :  BSD3
--
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Orphan instances we need to remain sane.
-----------------------------------------------------------------------------
module Text.Trifecta.Instances () where

#if !MIN_VERSION_ansi_wl_pprint(0,6,8)
import qualified Data.Semigroup               as Data
import           Text.PrettyPrint.ANSI.Leijen

instance Data.Semigroup Doc where
  (<>) = (<>)
#endif

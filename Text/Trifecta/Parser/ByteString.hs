{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Trifecta.Parser.ByteString
-- Copyright   :  (c) Edward Kmett 2011
-- License     :  BSD-style (see the LICENSE file)
--
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable (mptcs, fundeps)
--
-- Loading a file as a strict bytestring in one step.
--
-----------------------------------------------------------------------------


module Text.Trifecta.Parser.ByteString
    ( parseFromFile
    , parseFromFileEx
    ) where

import Control.Applicative
import Control.Monad (unless)
import Data.Semigroup
import Data.Foldable
import qualified Data.ByteString as B
import System.Console.Terminfo.PrettyPrint
import Text.Trifecta.Parser.Mark
import Text.Trifecta.Parser.Prim
import Text.Trifecta.Parser.Step
import Text.Trifecta.Rope.Delta
import Text.Trifecta.Parser.Result
import Data.Sequence as Seq
import qualified Data.ByteString.UTF8 as UTF8
import Text.Trifecta.Rope.Prim
import qualified Data.FingerTree as F


-- | @parseFromFile p filePath@ runs a parser @p@ on the
-- input read from @filePath@ using 'ByteString.readFile'. All diagnostic messages
-- emitted over the course of the parse attempt are shown to the user on the console.
--
-- > main = do
-- >   result <- parseFromFile numbers "digits.txt"
-- >   case result of
-- >     Nothing -> return ()
-- >     Just a  -> print $ sum a

parseFromFile :: Show a => Parser String a -> String -> IO (Maybe a)
parseFromFile p fn = do
  result <- parseFromFileEx p fn
  case result of
     Success xs a -> Just a  <$ unless (Seq.null xs) (displayLn (toList xs))
     Failure xs   -> Nothing <$ unless (Seq.null xs) (displayLn (toList xs))

-- | @parseFromFileEx p filePath@ runs a parser @p@ on the
-- input read from @filePath@ using 'ByteString.readFile'. Returns all diagnostic messages
-- emitted over the course of the parse and the answer if the parse was successful.
--
-- > main = do
-- >   result <- parseFromFileEx (many number) "digits.txt"
-- >   case result of
-- >     Failure xs -> unless (Seq.null xs) $ displayLn xs
-- >     Success xs a  ->
-- >       unless (Seq.null xs) $ displayLn xs
-- >       print $ sum a
-- >

parseFromFileEx :: Show a => Parser String a -> String -> IO (Result TermDoc a)
parseFromFileEx p fn = k <$> B.readFile fn where
  k i = starve
      $ feed (rope (F.fromList [LineDirective (UTF8.fromString fn) 0, strand i]))
      $ stepParser (fmap prettyTerm)
                   (why prettyTerm)
                   (release (Directed (UTF8.fromString fn) 0 0 0 0) *> p)
                   mempty
                   True
                   mempty
                   mempty

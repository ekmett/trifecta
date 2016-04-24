{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Main
( main
) where

import Control.Applicative

#if MIN_VERSION_base(4,7,0)
import Data.Either
#endif

#if __GLASGOW_HASKELL__ < 710
import Data.Monoid
#endif

import qualified Test.QuickCheck as Q

import Text.Parser.Char
import Text.Parser.Combinators

import Text.Trifecta.Parser
import Text.Trifecta.Result

import System.Exit

-- -------------------------------------------------------------------------- --
-- Main

main :: IO ()
main = mapM Q.quickCheckResult tests >>= \x -> case filter (not . passed) x of
    [] -> exitSuccess
    _ -> exitFailure
  where
    passed Q.Success{} = True
    passed _ = False

-- -------------------------------------------------------------------------- --
-- Tests

tests :: [Q.Property]
tests =
    [ Q.property prop_fail
    , Q.property prop_succeed
    , Q.property prop_notFollowedBy0
    , Q.property prop_notFollowedBy1
    , Q.property prop_notFollowedBy2
    , Q.property prop_notFollowedBy3
    ]

-- -------------------------------------------------------------------------- --
-- Properties

prop_fail :: String -> Bool
prop_fail = isLeft . parse (fail "fail" :: Parser ())

prop_succeed :: String -> Bool
prop_succeed = isRight . parse (mempty :: Parser ())

prop_notFollowedBy0 :: Char -> Char -> Bool
prop_notFollowedBy0 x y = either (\_ -> x == y) (/= y)
    $ parse (notFollowedBy (char y) *> anyChar) [x]

prop_notFollowedBy1 :: Char -> Bool
prop_notFollowedBy1 x = either (\_ -> x == x) (/= x)
    $ parse (notFollowedBy (char x) *> anyChar) [x]

prop_notFollowedBy2 :: String -> Char -> Bool
prop_notFollowedBy2 x y = isLeft
    $ parse (anyChar *> notFollowedBy (char y) *> char y) x

prop_notFollowedBy3 :: Char -> Bool
prop_notFollowedBy3 x = isRight
    $ parse (notFollowedBy (char x) <|> char x *> pure ()) [x]

-- -------------------------------------------------------------------------- --
-- Utils

parse :: Parser a -> String -> Either String a
parse p s = case parseString p mempty s of
    Failure e -> Left (show e)
    Success a -> Right a

#if !MIN_VERSION_base(4,7,0)
isLeft :: Either a b -> Bool
isLeft = either (const True) (const False)

isRight :: Either a b -> Bool
isRight = either (const False) (const True)
#endif

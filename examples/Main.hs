module Main (main) where

import RFC2616 (lumpy)
import System.Environment (getArgs)

main :: IO ()
main = mapM_ lumpy =<< getArgs

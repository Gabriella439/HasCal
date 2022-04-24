module Main where

import qualified Test.DocTest as DocTest
import qualified System.Environment as Environment

main :: IO ()
main = do
    args <- Environment.getArgs
    DocTest.mainFromCabal "HasCal" args

module Main where

-- Run this test with the command:
-- cabal test

import Test.Tasty
import Test.Tasty.HUnit

import MarketDataClientUtilsTests

main :: IO ()
main = defaultMain rqQueryStrTests
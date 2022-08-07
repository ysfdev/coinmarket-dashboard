{-# LANGUAGE QuasiQuotes, TypeApplications #-}

module TestCoinData where

import Data.Aeson ((.:))
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as AT
import Data.Vector (Vector)
import qualified Data.Vector as V

import CoinDataUtils
import CoinDataSample
import CoinData
import qualified MarketDataClient
import qualified MarketDataClientTypes as NT
-- import MarketDataTypes

toArrayFromSampleData :: String -> AT.Array
toArrayFromSampleData s = let 
  a = case A.decode (toBStr s) of 
    Just o -> AT.parseMaybe (.: toKey "data") o :: Maybe AT.Array
    _ -> Nothing in case a of
  Just a -> a
  _ -> V.empty

parseSampleCoinData :: String -> AT.Result Coin
parseSampleCoinData s = case A.decode (toBStr s) of
  Just o -> AT.parse AT.parseJSON o
  Nothing -> AT.Error "Failed to decode string as object"

findParseErrorsInSampleCoinListData :: String -> Vector (AT.Result String,AT.Result Coin)
findParseErrorsInSampleCoinListData s = let
  getName :: AT.Value -> AT.Result String
  getName v = case v of AT.Object o -> AT.parse (.: toKey "name") o; _ -> AT.Error "Didn't get an object array from sample data" in
  V.filter (\(_,rx) -> case rx of AT.Error s -> True; _ -> False) $ 
  V.map (\v -> (getName v,AT.parse (AT.parseJSON @Coin) v)) $ toArrayFromSampleData s

formatResults :: Vector (AT.Result String,AT.Result Coin) -> String
formatResults = foldr
  (\(rs,rc) acc-> let
  errStr = (\s-> "error - " <> s <> "\n")
  n = "name: " <> case rs of AT.Error s -> errStr s;AT.Success s-> s <> ", "
  c = "coin: " <> case rc of AT.Error s -> errStr s;AT.Success c-> show c in
  n <> c <> acc) ""

testDataClientParams = NT.QueryParams 0 0
testDataClientIntegration :: Bool
testDataClientIntegration = False
-- testDataClientIntegration = True
-- testDataClientIntegration = fetchLatestListings testDataClientIntegration
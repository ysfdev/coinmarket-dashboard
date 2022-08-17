-- {-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module TestCoinData where

import Data.Aeson ((.:))
import Data.Aeson
import Data.Aeson.Types
import Data.Vector (Vector)
import qualified Data.Vector as V

import CoinDataUtils
import CoinDataSample
import CoinData
import MarketDataClient
import qualified MarketDataClientTypes as NT
-- import MarketDataTypes

toArrayFromSampleData :: String -> Array
toArrayFromSampleData s = let 
  a = case decode (toBStr s) of 
    Just o -> parseMaybe (.: toKey "data") o :: Maybe Array
    _ -> Nothing in case a of
  Just a -> a
  _ -> V.empty

parseSampleCoinData :: String -> Result Coin
parseSampleCoinData s = case decode (toBStr s) of
  Just o -> parse parseJSON o
  Nothing -> Error "Failed to decode string as object"

displayParseErrorsFromString :: String -> IO ()
displayParseErrorsFromString s = putStr.formatResults.findParseErrors $ toArrayFromSampleData s

findParseErrors :: Array -> Vector (Result String,Result Coin)
findParseErrors a = let
  getName :: Value -> Result String
  getName v = case v of Object o -> parse (.: toKey "name") o; _ -> Error "Failed to parse an object from the sample data" in
  V.filter (\(_,rx) -> case rx of Error s -> True; _ -> False) $ 
  V.map (\v -> (getName v,parse (parseJSON @Coin) v)) $ a

formatResults :: Vector (Result String,Result Coin) -> String
formatResults = foldr
  (\(rs,rc) acc-> let
  errStr = (\s-> "error - " <> s <> "\n")
  n = "name: " <> case rs of Error s -> errStr s;Success s-> s <> ", "
  c = "coin: " <> case rc of Error s -> errStr s;Success c-> show c in
  n <> c <> acc) ""

getRspBody :: NT.DataResponse -> Array
getRspBody = NT.body

tdcParams = NT.QueryParams 0 100
testDataClientIntegration :: IO Bool
testDataClientIntegration =
  fetchLatestListings tdcParams >>= \rsp -> let
  status = NT.status rsp
  msg = NT.message rsp
  body = NT.body rsp
  errStr = formatResults . findParseErrors $ body in
  if NT.Successful /= status then
    putStrLn ("Reuqest failed with status: " <> show status) >>
    putStrLn msg >>
    return False
  else if null body then
    putStrLn "Response body was empty" >>
    return False
  else if not $ null errStr then
    putStr errStr >>
    return False
  else
    putStrLn "No parse errors found!" >>
    return True

-- {-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ExplicitForAll #-}

module TestCoinData where



import CoinDataUtils
import CoinDataSample
import CoinData
import MarketDataClient
import qualified MarketDataClientTypes as NT

import Data.Text (Text)
import qualified Data.Text as T
import Data.Aeson
import Data.Aeson.Types
import Data.Vector (Vector)
import qualified Data.Vector as V
-- import MarketDataTypes

_qPropKey = ["quote","USD","market_cap"]

_sampleCoin1 = decode (toBStr _sampleCoinData1) :: Maybe Coin
_sampleCoin2 = decode (toBStr _sampleCoinData2) :: Maybe Coin

testParametricParser :: String -> (Coin -> Object) -> Result String
testParametricParser key co = case _sampleCoin2 of
  Nothing -> Error "Couldn't parse coin from sample data"
  Just c -> case parse (.:: key) (co c) :: Result Value of
    Error err -> Error err
    Success v -> case v of
      Object o -> Success $ fromBStr $ encode o
      Array a -> Success $ fromBStr $ encode a
      String s -> Success $ T.unpack s
      Number n -> Success $ show n
      Bool b -> Success $ show b
      Null -> Success ""


getQuoteVal :: String -> IO Double
getQuoteVal s = let
  ks = _qPropKey
  nextElem v [] = Error "Empty path element array"
  nextElem v [k] =  case parse (.:: k) v :: Result Double of
    Success d -> Success d
    Error err -> Error err
  nextElem v (k:ks) = case parse (.:: k) v of
    Success v -> case v of
      Object o -> nextElem o ks
      _ -> Error "incorrect value type from path element"
    Error err -> Error err
  a = case decode $ toBStr s of 
    Just o -> nextElem o ks
    _ -> Error "Couldn't parse object from sample data" in 
  case a of
  Success a -> return a
  Error err -> putStrLn err >> return (-1)


-- parseSampleCoinData :: String -> Result Coin
-- parseSampleCoinData s = case decode (toBStr s) of
--   Just o -> parse parseJSON o
--   Nothing -> Error "Failed to decode string as object"

displayParseErrorsFromString :: String -> IO ()
displayParseErrorsFromString s = putStr.formatResults.findParseErrors $ toArrayFromString s

testParsingCoinListSampleData :: IO Bool
testParsingCoinListSampleData = let
  body = toArrayFromString _sampleCoinListData
  errStr = formatResults . findParseErrors $ body in
  if null body then
    putStrLn "Failed to decode body was empty" >>
    return False
  else if not $ null errStr then
    putStr errStr >>
    return False
  else
    putStrLn "No parse errors found!" >>
    return True

findParseErrors :: Result (Vector Coin) -> Vector (Result String,Result Coin)
findParseErrors rvc = let
  getName :: Object -> Result String
  getName o = parse (.:: "name") o in
  case rvc of
    Error err -> V.fromList [(Error "", Error err)];
    Success vc ->
      V.filter (\(_,rx) -> case rx of Error s -> True; _ -> False) $ 
      V.map (\c -> (getName $ _coinJSON c, Success c)) vc

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
  errStr = formatResults . findParseErrors $ V.mapM (parse parseJSON) body in
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

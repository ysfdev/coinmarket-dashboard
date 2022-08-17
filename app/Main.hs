module Main where

import MarketDataClient (fetchLatestListings)
import MarketDataClientTypes (QueryParams (QueryParams))
import MarketDataClientUtils (buildReqQ)
import Views

main :: IO ()
main = do
  marketData <- fetchLatestListings $ buildReqQ 0 10
  putStrLn "MarketData:"
  print marketData
  putStrLn "Welcome, CoinMarket Dashboard"
  putStrLn ""
  printDashoardNew

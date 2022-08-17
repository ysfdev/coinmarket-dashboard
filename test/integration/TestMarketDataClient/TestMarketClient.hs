module TestMarketClient where

import MarketDataClient
import MarketDataClientUtils
import MarketDataClientTypes

testFetchDataSandbox :: IO DataResponse
testFetchDataSandbox = do 
    marketData <- fetchData ReqParams {
            host=coinMarketSandboxAPIServerHost,
            endpoint=cryptoLatestListingsEndpoint
        }
    putStrLn "MarketData:"
    print marketData
    return marketData
     




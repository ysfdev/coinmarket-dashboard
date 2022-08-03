
{-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE ScopedTypeVariables #-}

module MarketDataClient where

import Data.Aeson as JSON
import qualified Network.HTTP.Simple as Http
import Control.Monad (MonadPlus(mzero))
import Data.String (IsString(fromString))

-- CoinMarketCap API Details
coinMarketAPIServerHost = "https://pro-api.coinmarketcap.com"
coinMarketSandboxAPIServerHost = "https://sandbox-api.coinmarketcap.com"
coinMarketAPIKey = "b54bcf4d-1bca-4e8e-9a24-22ff2c3d462c" -- ProdKey = "882a4874-8e57-4674-8b9d-7509be621718"
cryptoLatestListingsEndpoint = "/v1/cryptocurrency/listings/latest"

-- Data Types Definitions --
type Endpoint = String
type Limit = Int
type StatusCode = String
type Message = String
type RawReqBody = String

data Method = GET | POST | PUT 
data QueryParams = QueryParams { limit :: Limit } 
data RequestParams = RequestParams {
  method :: Method,
  endpoint :: Endpoint,
  params :: QueryParams
}
data Status = Successful | Forbidden | BadRequest | InternalServerError | TooManyRequests deriving Show
data DataResponse = DataResponse {
   status :: Status,
   message :: Message, --- (error_message from API response)
   body :: RawReqBody -- Raw API Response Body .Data[] JSON String
} deriving Show

instance FromJSON DataResponse where
    parseJSON (Object v) = do
      body' <- v .: "data"
      message' <- v .: "status"
      return $ DataResponse { body = body', message = message' }
    parseJSON _ = mzero

-- METHODS --

-- fetchData queries market data from custom endpoints
-- TODO: verify failure of request
fetchData :: RequestParams -> IO DataResponse 
fetchData rp = do
 let request
      = Http.setRequestMethod (fromString $ methodStr $ method rp)
      $ Http.setRequestPath (fromString $ endpoint rp)
      $ Http.setRequestHost (fromString coinMarketSandboxAPIServerHost)
      $ Http.setRequestPort 443
      $ Http.addRequestHeader (fromString "X-CMC_PRO_API_KEY") (fromString coinMarketAPIKey)
      $ Http.defaultRequest
 response <- Http.httpJSON request :: IO (Http.Response DataResponse)
--  print "Request URL:" Http.parseRequest request 
 putStrLn $ "StatusCode: " ++ show (Http.getResponseStatusCode response)
 print $ "Body" ++ show response
 return $ Http.getResponseBody response

-- fetchLatestListing returns a paginated list of all active cryptocurrencies with latest market data
fetchLatestListings :: QueryParams ->  DataResponse
fetchLatestListings =  error "pending"
   where 
      req = RequestParams { method=GET, endpoint=cryptoLatestListingsEndpoint, params=QueryParams {limit=10}}

methodStr :: Method -> String
methodStr GET  = "GET"
methodStr PUT  = "PUT"
methodStr POST = "POST"
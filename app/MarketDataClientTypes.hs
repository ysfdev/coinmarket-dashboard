{-# LANGUAGE OverloadedStrings #-}
module MarketDataClientTypes where

import Control.Exception (SomeException (SomeException), handle)
import Control.Monad (MonadPlus (mzero))
import Control.Monad.IO.Class (liftIO)
import Data.Aeson as JSON
import Data.String (IsString (fromString))
import qualified GHC.Exts as Aeson
import Network.HTTP.Client.Conduit (HttpException)
import qualified Network.HTTP.Conduit as Http
import qualified Network.HTTP.Simple as Http
import qualified Data.Vector as V

-- CoinMarketCap API Details
-- Hosts ---
coinMarketAPIServerHost = "https://pro-api.coinmarketcap.com"
coinMarketAPIKey :: [Char]
coinMarketAPIKey = "882a4874-8e57-4674-8b9d-7509be621718"
coinMarketSandboxAPIServerHost = "https://sandbox-api.coinmarketcap.com"
coinMarketSandboxAPIKey = "b54bcf4d-1bca-4e8e-9a24-22ff2c3d462c"

-- Endpoints --
cryptoLatestListingsEndpoint = "/v1/cryptocurrency/listings/latest"

-- Data Types Definitions --
type Endpoint = String

type Host = String

type APIKey = String

type StatusCode = String

type Message = String

type RawReqBody = String

type RLimit = Int

type RStart = Int

data QueryParams = QueryParams
  { -- start allows to offset the start (1-based index) of the paginated list of items to return.
    start :: RStart,
    -- limit allows to specify the number of results to return. Default: 100
    limit :: RLimit
  } deriving (Show, Eq)

data ReqParams = ReqParams
  { host :: Host,
    endpoint :: Endpoint,
    apiKey :: APIKey,
    query :: QueryParams
  } deriving (Show, Eq)

data Status
  = Successful
  | Unauthorized
  | Forbidden
  | BadRequest
  | InternalServerError
  | TooManyRequests
  | RequestRedirected
  | FailToSendReq
  deriving (Show, Eq)

data DataResponse = DataResponse
  { status :: Status,
    message :: Message, --- (error_message from API response)
    body :: Array  -- API response.data body
  }
  deriving (Show)

data APIResponse = APIResponse
  { rmessage :: Message,
    rdata :: Array
  }
  deriving (Show)

instance FromJSON APIResponse where
  parseJSON (Object v) = do
    data' <- v .:? "data" .!= V.empty
    -- TODO extract status.error_message
    -- status' <- v .:? "status" .!= Object
    -- msg'  <- status' .:? "error_message"
    return $ APIResponse {rdata = data', rmessage = ""}
  parseJSON _ = mzero

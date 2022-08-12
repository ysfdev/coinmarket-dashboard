{-# LANGUAGE OverloadedStrings #-}

module MarketDataClientUtils
  ( resStatus,
    resData,
    reqExHandler,
    buildReqQ,
    rHost,
    rEndpoint,
    rqQueryStr,
  )
where

import Control.Exception (SomeException (SomeException), handle)
import Control.Monad (MonadPlus (mzero))
import Control.Monad.IO.Class (liftIO)
import Data.Aeson as JSON
import Data.String (IsString (fromString))
import qualified GHC.Exts as Aeson
import MarketDataClientTypes
import Network.HTTP.Client.Conduit (HttpException)
import qualified Network.HTTP.Conduit as Http
import qualified Network.HTTP.Simple as Http
import Control.Applicative (Alternative(empty))

buildReqQ :: RStart -> Limit -> QueryParams
buildReqQ s l = QueryParams {start = s, limit = l}

-- resStatus extracts the Status of Http Response
resStatus :: Http.Response a -> IO Status
resStatus r = do return $ codeToStatus s
  where
    s = Http.getResponseStatusCode r

-- resData builds a DataResponse from APIResponse
resData :: Http.Response APIResponse -> IO DataResponse
resData r = do
  status' <- resStatus r
  return
    DataResponse
      { status = status',
        message = rmessage dr,
        body = rdata dr
      }
  where
    dr = Http.getResponseBody r

-- codeToStatus maps known API response codes into statuses
codeToStatus :: Int -> Status
codeToStatus c
  | c >= 200 && c < 300 = Successful
  | c >= 300 && c < 400 = RequestRedirected
  | c == 429 = TooManyRequests
  | c == 401 = Unauthorized
  | c == 403 = Forbidden
  | c >= 400 && c < 500 = BadRequest
  | otherwise = InternalServerError

-- rHost extracts host from given params or defaults to sandbox API host
rHost :: IsString p => ReqParams -> p
rHost rp
  | null $ host rp = fromString coinMarketSandboxAPIServerHost
  | otherwise = fromString $ host rp

-- rEndpoint extracts endpoint from params or defaults to latest listings endpoint
rEndpoint :: IsString p => ReqParams -> p
rEndpoint rp
  | null $ endpoint rp = fromString cryptoLatestListingsEndpoint
  | otherwise = fromString $ endpoint rp

-- rqParams extracts and parses request query strings
rqQueryStr :: ReqParams -> Http.Query
rqQueryStr ReqParams {query = q} = [("limit", Just l)]
  where
    l = fromString . show $ rLimit q

rLimit :: QueryParams -> Limit
rLimit QueryParams {limit = l}
  | l > 0 = l
  | otherwise = 100

-- reqExHandler handles exceptions sending a HTTP request
reqExHandler :: SomeException -> IO DataResponse
reqExHandler err = do
  putStrLn $ "Error sending request: " ++ show err
  return DataResponse {status = FailToSendReq, message = show err, body = empty}

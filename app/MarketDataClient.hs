module MarketDataClient
  ( fetchData,
    fetchLatestListings,
    doMarketReq,
  )
where

import Control.Exception (SomeException (SomeException), handle)
import Control.Monad (MonadPlus (mzero))
import Control.Monad.IO.Class (liftIO)
import Data.Aeson as JSON
import Data.String (IsString (fromString))
import qualified GHC.Exts as Aeson
import MarketDataClientTypes
import MarketDataClientUtils
import Network.HTTP.Client.Conduit (HttpException)
import qualified Network.HTTP.Conduit as Http
import qualified Network.HTTP.Simple as Http

-- Public METHODS --
-- fetchData queries market data from custom endpoints
fetchData :: ReqParams -> IO DataResponse
fetchData rp = handle reqExHandler req
  where
    req = do
      res <- doMarketReq rp
      resData res

-- fetchLatestListing returns a paginated list of all active cryptocurrencies with latest market data
fetchLatestListings :: QueryParams -> IO DataResponse
fetchLatestListings q =
  fetchData
    ReqParams
      { host = coinMarketSandboxAPIServerHost,
        apiKey = coinMarketSandboxAPIKey,
        endpoint = cryptoLatestListingsEndpoint,
        query = q
      }

-- doMarketReq constructs and sends a coin marketcap API request
-- TODO: handle JSONConversionException
doMarketReq :: ReqParams -> IO (Http.Response APIResponse)
doMarketReq rp = do
  request' <- Http.parseRequest (rHost rp)
  let request =
        Http.setRequestPath (rEndpoint rp) $
        Http.setRequestQueryString (rqQueryStr rp) $
        Http.setRequestSecure True $
        Http.addRequestHeader (fromString "X-CMC_PRO_API_KEY") (fromString $ apiKey rp) $
        Http.addRequestHeader
          (fromString "Accept-Encoding")
          (fromString "deflate, gzip")
        request'
  print $ "Sending new market API request to: " ++ rHost rp ++ "" ++ rEndpoint rp
  print request
  response <- Http.httpJSON request
  putStrLn $ "Response status code: " ++ show (Http.getResponseStatusCode response)
  return response
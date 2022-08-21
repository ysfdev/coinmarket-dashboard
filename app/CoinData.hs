{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}

module CoinData 
(
  Coin (..)
-- , coinFromJSON
-- , coinListFromArray
-- , CoinQuote (..)
-- , coinQuoteFromJSON
, CoinProperty (..)
, CoinLookupParams (..)
, CoinLookupResult (..)
, GetCoinsResult (..)
, coinLookUp
, top10Coins
) where

import Data.Map
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Maybe
import Data.Aeson
import Data.Aeson.Types

import CoinDataUtils
import CoinDataSample
import CoinDataTypes ( Coin(..), CoinProperty(..) )

import MarketDataClient

-- coinQuoteFromJSON :: String -> Maybe CoinQuote
-- coinQuoteFromJSON s = decode $ toBStr s

-- coinListFromArray :: Array -> Vector (Maybe Coin)
-- coinListFromArray = V.map (parseMaybe parseJSON)

-- coinListFromJSON :: String -> Maybe (Vector Coin)
-- coinListFromJSON s = case decode (toBStr s) of
--   Nothing -> Nothing
--   (Just q) -> coinListFromArray q

-- coinFromJSON :: String -> Maybe Coin
-- coinFromJSON s = decode $ toBStr s

data CoinLookupParams = CoinLookupParams 
  {
    _clpCoinName :: Maybe String  -- (text search based on name)
  , _clpCoinSymbol :: Maybe String -- (text search based on ticker)
  , _clpCoinSlug :: Maybe String -- (text search based on slug)
  , _clpCoinId :: Maybe Int
  , _clpCoinCmcRank :: Maybe Int
  }

data CoinLookupResult = ClrCoin {_clrGetCoin :: Maybe Coin} | ClrNotFoundError | ClrUnexpectedError

data GetCoinsParams = GetCoinsParams
  {
    _tcpLimit :: Maybe Int
  , _tcpSortBy :: Maybe CoinProperty
  , _tcpFilterBy :: Maybe CoinProperty
  }

data GetCoinsResult = GcrCoinList {_gcrGetList :: Vector Coin} | GcrNotFoundError | GcrUnexpectedError deriving Show


----- =============== End Pure Code =============== -----


-- Should perform full text search look up on local coin data store based on _coinSlug (coin name)
coinLookUp :: CoinLookupParams -> IO CoinLookupResult
coinLookUp _ = return ClrNotFoundError

-- Fetch from DB
getCoins :: GetCoinsParams -> IO GetCoinsResult
getCoins _ = return GcrNotFoundError

-- Fetch the top ten coins from the local store
top10Coins :: IO GetCoinsResult
-- top10Coins = getCoins $ GetCoinsParams (Just 10) Nothing Nothing
-- top10Coins = let 
--   a = case decode (toBStr _sampleCoinListData) of 
--     Just o -> parseMaybe (.: toKey "data") o :: Maybe Array
--     _ -> Nothing in case a of
--   Just a -> case V.mapM (parseMaybe (parseJSON @Coin)) a of
--     Just va -> return $ GcrCoinList (V.take 10 va)
--     Nothing -> return $ GcrCoinList V.empty
--   _ -> return $ GcrCoinList V.empty

top10Coins = let 
  a = case decode (toBStr _sampleCoinListData) of 
    Just o -> parseMaybe (.:: "data") o :: Maybe (Vector Coin)
    _ -> Nothing in case a of
  Just va -> return $ GcrCoinList va
  _ -> return GcrUnexpectedError
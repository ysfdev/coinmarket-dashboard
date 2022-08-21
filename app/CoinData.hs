{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}

module CoinData 
(
  Coin (..)
, coinFromJSON
, coinListFromArray
, CoinQuote (..)
, coinQuoteFromJSON
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

import MarketDataClient

data CoinProperty
  = CoinId
  | CoinName
  | CoinSymbol
  | CoinSlug
  | CoinCmcRank
  | CoinNumMarketPairs
  | CoinCirculatingSupply
  | CoinTotalSupply
  | CoinMaxSupply
  | CoinLastUpdated
  | CoinDateAdded
  | CoinTags
  | CoinSelfReportedCirculatingSupply
  | CoinSelfReportedMarketCap
  | CoinQPriceUSD
  | CoinQVolume24H
  | CoinQVolumeChange24h
  | CoinQPercentChange1h
  | CoinQPercentChange24h
  | CoinQPercentChange7d
  | CoinQMarketCap
  | CoinQMarketCapDominan
  | CoinQFullyDilutedMarketCap
  | CoinQLastUpdated
  deriving (Eq,Ord,Enum,Show)

data CoinQuote = CoinQuote
  {
   _coinQPrice :: Double
  , _coinQVolume24H :: Double
  , _coinQVolumeChange24h :: Double
  , _coinQPercentChange1h :: Double
  , _coinQPercentChange24h :: Double
  , _coinQPercentChange7d :: Double
  , _coinQMarketCap :: Double
  , _coinQMarketCapDominance :: Double
  , _coinQFullyDilutedMarketCap :: Double
  , _coinQLastUpdated :: String
  } deriving Show

instance FromJSON CoinQuote where
  parseJSON =
    withObject "quote" $ \v -> CoinQuote
    <$> annotatedParser v "price"
    <*> annotatedParser v "volume_24h"
    <*> annotatedParser v "volume_change_24h"
    <*> annotatedParser v "percent_change_1h"
    <*> annotatedParser v "percent_change_24h"
    <*> annotatedParser v "percent_change_7d"
    <*> annotatedParser v "market_cap"
    <*> annotatedParser v "market_cap_dominance"
    <*> annotatedParser v "fully_diluted_market_cap"
    <*> annotatedParser v "last_updated"

coinQuoteFromJSON :: String -> Maybe CoinQuote
coinQuoteFromJSON s = decode $ toBStr s

data Coin = Coin
  {
    _coinId :: Int
  , _coinName :: String
  , _coinSymbol :: String
  , _coinSlug :: String
  , _coinCmcRank :: Int
  , _coinNumMarketPairs :: Int
  , _coinCirculatingSupply :: Double
  , _coinTotalSupply :: Double
  , _coinMaxSupply :: Double
  , _coinLastUpdated :: String
  , _coinDateAdded :: String
  , _coinTags :: [String]
  , _coinSelfReportedCirculatingSupply :: Maybe Double
  , _coinSelfReportedMarketCap :: Maybe Double
  , _coinQuoteMap :: Map String CoinQuote -- keyed on the quote unit e.g. USD, BTC, etc...
  } deriving (Show)

instance Eq Coin where
  (==) a b = _coinId a == _coinId b 

instance FromJSON Coin where
  parseJSON =
    withObject "Coin" $ \v -> Coin
    <$> annotatedParser v "id"
    <*> annotatedParser v "name"
    <*> annotatedParser v "symbol"
    <*> annotatedParser v "slug"
    <*> annotatedParser v "cmc_rank"
    <*> annotatedParser v "num_market_pairs"
    <*> annotatedParser v "circulating_supply"
    <*> annotatedParser v "total_supply"
    <*> annotatedParser v "max_supply"
    <*> annotatedParser v "last_updated"
    <*> annotatedParser v "date_added"
    <*> annotatedParser v "tags"
    <*> annotatedParser v "self_reported_circulating_supply"
    <*> annotatedParser v "self_reported_market_cap"
    <*> annotatedParser v "quote"

coinListFromArray :: Array -> Vector (Maybe Coin)
coinListFromArray = V.map (parseMaybe parseJSON)

-- coinListFromJSON :: String -> Maybe (Vector Coin)
-- coinListFromJSON s = case decode (toBStr s) of
--   Nothing -> Nothing
--   (Just q) -> coinListFromArray q

coinFromJSON :: String -> Maybe Coin
coinFromJSON s = decode $ toBStr s

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
top10Coins = let 
  a = case decode (toBStr _sampleCoinListData) of 
    Just o -> parseMaybe (.: toKey "data") o :: Maybe Array
    _ -> Nothing in case a of
  Just a -> case V.mapM (parseMaybe (parseJSON @Coin)) a of
    Just va -> return $ GcrCoinList (V.take 10 va)
    Nothing -> return $ GcrCoinList V.empty
  _ -> return $ GcrCoinList V.empty

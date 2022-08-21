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
   _coinQPrice :: Either String Double
  , _coinQVolume24H :: Either String Double
  , _coinQVolumeChange24h :: Either String Double
  , _coinQPercentChange1h :: Either String Double
  , _coinQPercentChange24h :: Either String Double
  , _coinQPercentChange7d :: Either String Double
  , _coinQMarketCap :: Either String Double
  , _coinQMarketCapDominance :: Either String Double
  , _coinQFullyDilutedMarketCap :: Either String Double
  , _coinQLastUpdated :: Either String String
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

coinQuoteFromJSON :: Either String String -> Maybe CoinQuote
coinQuoteFromJSON s = case s of
  Left err -> Nothing
  Right pass -> decode $ toBStr pass

data Coin = Coin
  {
    _coinId :: Either String Int
  , _coinName :: Either String String
  , _coinSymbol :: Either String String
  , _coinSlug :: Either String String
  , _coinCmcRank :: Either String Int
  , _coinNumMarketPairs :: Either String Int
  , _coinCirculatingSupply :: Either String Double
  , _coinTotalSupply :: Either String Double
  , _coinMaxSupply :: Either String Double
  , _coinLastUpdated :: Either String String
  , _coinDateAdded :: Either String String
  , _coinTags :: Either String [String]
  , _coinSelfReportedCirculatingSupply :: Either String Double
  , _coinSelfReportedMarketCap :: Either String Double
  , _coinQuoteMap :: Either String (Map String CoinQuote) -- keyed on the quote unit e.g. USD, BTC, etc...
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

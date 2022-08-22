module CoinDataTypes where

import Data.Text (Text)
import Data.Map (Map)
import Data.Vector (Vector)
import Data.Aeson
import Data.Aeson.Types

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
  | CoinQPrice
  | CoinQVolume24H
  | CoinQVolumeChange24h
  | CoinQPercentChange1h
  | CoinQPercentChange24h
  | CoinQPercentChange7d
  | CoinQMarketCap
  | CoinQMarketCapDominance
  | CoinQFullyDilutedMarketCap
  | CoinQLastUpdated
  deriving (Eq,Ord,Enum,Show)

data Coin = Coin
  {
    _coinJSON :: Object
  , _quoteJSON :: Map String Object
  } deriving (Show)

data CoinLookupParams = CoinLookupParams 
  {
    _clpCoinName :: Maybe String  -- (text search based on name)
  , _clpCoinSymbol :: Maybe String -- (text search based on ticker)
  , _clpCoinSlug :: Maybe String -- (text search based on slug)
  , _clpCoinId :: Maybe Int
  , _clpCoinCmcRank :: Maybe Int
  } deriving (Show)

data CoinLookupResult = ClrCoin {_clrGetCoin :: Maybe Coin} | ClrNotFoundError | ClrUnexpectedError
  deriving (Show)

data GetCoinsParams = GetCoinsParams
  {
    _tcpLimit :: Maybe Int
  , _tcpSortBy :: Maybe CoinProperty
  , _tcpFilterBy :: Maybe CoinProperty
  }
  deriving (Show)

data GetCoinsResult = GcrCoinList {_gcrGetList :: Vector Coin} | GcrNotFoundError | GcrUnexpectedError
  deriving (Show)
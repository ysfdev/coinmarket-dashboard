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
  | CoinQuote
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

data CoinTextSearchStyle = 
    CtssBegin     -- Begins with search term
  | CtssEnd       -- Ends with search term
  | CtssContain   -- Contains search term

data CoinLookupSearchData = 
  -- (text search based on name)
    ClsdName   { _clsdCoinName :: String }
  -- (text search based on ticker symbol) 
  | ClsdSymbol { _clsdCoinSymbol :: String }
   -- (text search based on slug)
  | ClsdSlug   { _clsdCoinSlug :: String }
  deriving (Show)

data CoinLookupParams = CoinLookupParams 
  {
    clpSearchData :: CoinLookupSearchData
  , clpSearchStyle :: CoinTextSearchStyle
  }

data CoinLookupResult = ClrCoin {_clrGetCoin :: Coin} | ClrNotFoundError | ClrUnexpectedError
  deriving (Show)

data GetCoinsParams = GetCoinsParams
  {
    _gcpLimit :: Int
  , _gcpSortBy :: CoinProperty
  , _gcpFilterBy :: CoinProperty
  , _gcpUnit :: String
  }
  deriving (Show)

data GetCoinsResult = GcrCoinList {_gcrGetList :: Vector Coin} | GcrNotFoundError | GcrUnexpectedError
  deriving (Show)
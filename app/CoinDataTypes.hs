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
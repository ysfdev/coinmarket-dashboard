-- {-# LANGUAGE TypeApplications #-}

module CoinDataUtils where

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Data.Aeson
import Data.Aeson.Types
import qualified Data.Aeson.Key as K
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Data.Map (Map)
import qualified Data.Map as M
import Data.Vector (Vector)
import qualified Data.Aeson.Key as T
import CoinDataTypes
import qualified CoinDataTypes as CoinId

fromBStr :: BL.ByteString -> String
fromBStr = TL.unpack.TLE.decodeUtf8

toBStr :: String -> BL.ByteString
toBStr = TLE.encodeUtf8.TL.pack

toText :: String -> T.Text
toText = T.pack

toKey :: String -> Key
toKey = K.fromString

toParser :: FromJSON a => Object -> String -> Parser a
toParser o s = o .: toKey s

toArrayFromString :: String -> Result (Vector Coin)
toArrayFromString s =
  case decode (toBStr s) of 
    Just o -> parse (.:: "data") o :: Result (Vector Coin)
    _ -> Error "Failed to parse object from sample input string"

-- Useful for nested object parsing
infixl 8 .->
(.->) :: FromJSON a => Parser Object -> String -> Parser a
(.->) parser key =
  parser >>= \obj ->
  obj .:: key

-- | Convenience method for annotating the error message output by a parser
--  with the field name that will be looked up in the supplied object
-- Example:
--
-- > parseJSON (Object o) = annotatedParser o "fieldName"
infixl 8 .::
(.::) :: FromJSON a => Object -> String -> Parser a
(.::) = annotatedParser
annotatedParser :: FromJSON a => Object -> String -> Parser a
annotatedParser obj fieldName = 
  modifyFailure ("Error parsing field: \"" <> fieldName <> "\" " ++)
  (obj .: toKey fieldName)

instance FromJSON Coin where
  parseJSON = withObject "Coin" $ \o -> Coin o <$> o .:: "quote"

-- Coin Property Getters
_coinId :: Coin -> Result Int
_coinId c = _coinPropVal c CoinId
_coinName :: Coin -> Result String
_coinName c = _coinPropVal c CoinName
_coinSymbol :: Coin -> Result String
_coinSymbol c = _coinPropVal c CoinSymbol
_coinSlug :: Coin -> Result String
_coinSlug c = _coinPropVal c CoinSlug
_coinCmcRank :: Coin -> Result Int
_coinCmcRank c = _coinPropVal c CoinCmcRank
_coinNumMarketPairs :: Coin -> Result Int
_coinNumMarketPairs c = _coinPropVal c CoinNumMarketPairs
_coinCirculatingSupply :: Coin -> Result Double
_coinCirculatingSupply c = _coinPropVal c CoinCirculatingSupply
_coinTotalSupply :: Coin -> Result Double
_coinTotalSupply c = _coinPropVal c CoinTotalSupply
_coinMaxSupply :: Coin -> Result Double
_coinMaxSupply c = _coinPropVal c CoinMaxSupply
_coinLastUpdated :: Coin -> Result String
_coinLastUpdated c = _coinPropVal c CoinLastUpdated
_coinDateAdded :: Coin -> Result String
_coinDateAdded c = _coinPropVal c CoinDateAdded
_coinTags :: Coin -> Result [String]
_coinTags c = _coinPropVal c CoinTags
_coinSelfReportedCirculatingSupply :: Coin -> Result Double
_coinSelfReportedCirculatingSupply c = _coinPropVal c CoinSelfReportedCirculatingSupply
_coinSelfReportedMarketCap :: Coin -> Result Double
_coinSelfReportedMarketCap c = _coinPropVal c CoinSelfReportedMarketCap
_coinQuoteMap :: Coin -> Result (Map String Object) -- keyed on the quote unit e.g. USD, BTC, etc...
_coinQuoteMap c = Success $ _quoteJSON c

-- Quote Property Getters
_coinQPrice :: Coin -> CoinQuoteUnit -> Result Double
_coinQPrice c unit = _coinQPropVal c unit CoinQPrice
_coinQVolume24H :: Coin -> CoinQuoteUnit -> Result Double
_coinQVolume24H c unit = _coinQPropVal c unit CoinQVolume24H
_coinQVolumeChange24h :: Coin -> CoinQuoteUnit -> Result Double
_coinQVolumeChange24h c unit = _coinQPropVal c unit CoinQVolumeChange24h
_coinQPercentChange1h :: Coin -> CoinQuoteUnit -> Result Double
_coinQPercentChange1h c unit = _coinQPropVal c unit CoinQPercentChange1h
_coinQPercentChange24h :: Coin -> CoinQuoteUnit -> Result Double
_coinQPercentChange24h c unit = _coinQPropVal c unit CoinQPercentChange24h
_coinQPercentChange7d :: Coin -> CoinQuoteUnit -> Result Double
_coinQPercentChange7d c unit = _coinQPropVal c unit CoinQPercentChange7d
_coinQMarketCap :: Coin -> CoinQuoteUnit -> Result Double
_coinQMarketCap c unit = _coinQPropVal c unit CoinQMarketCap
_coinQMarketCapDominance :: Coin -> CoinQuoteUnit -> Result Double
_coinQMarketCapDominance c unit = _coinQPropVal c unit CoinQMarketCapDominance
_coinQFullyDilutedMarketCap :: Coin -> CoinQuoteUnit -> Result Double
_coinQFullyDilutedMarketCap c unit = _coinQPropVal c unit CoinQFullyDilutedMarketCap
_coinQLastUpdated :: Coin -> CoinQuoteUnit -> Result String
_coinQLastUpdated c unit = _coinQPropVal c unit CoinQLastUpdated

-- Coin key value getters
_coinPropVal :: FromJSON a => Coin -> CoinProperty -> Result a
_coinPropVal c cp = __coinPropVal (_coinJSON c) (case _coinPropStr cp of Just s -> s;_->"")

_coinQPropVal :: FromJSON a => Coin -> CoinQuoteUnit -> CoinProperty -> Result a
_coinQPropVal c unit cp = case M.lookup unit (_quoteJSON c)  of
  Just o -> __coinPropVal o (case _coinQPropStr cp of Just s -> s;_->"")
  Nothing -> Error $ "Couldn't find requested unit in quote map \"" <> unit <> "\""

__coinPropVal :: FromJSON a => Object -> String -> Result a
-- __coinPropVal o k = case parse (.:: k) o of Success v -> Just v; Error err -> Nothing
__coinPropVal o k = parse (.:: k) o

__coinPropValStr :: Object -> CoinFieldSchema -> Result String
__coinPropValStr o schema = 
    case parse (.:: _cfsFieldName schema) o :: Result Value of
    Error err -> case _cfsRequired schema of 
      CfRequired -> Error err
      CfOptional -> Success "null" -- return "null" for optional fields
    Success v -> case v of
      Object o -> Success $ show $ fromBStr $ encode o
      Array a -> Success $ show $ fromBStr $ encode a
      String s -> Success $ show $ T.unpack s
      Number n -> Success $ 
        if _cfsStorage schema == CstInteger then show (floor n) -- make sure int types have no decimals
        else show n
      Bool b -> Success $ show b
      Null -> Success "null"

_coinPropStr :: CoinProperty -> Maybe String
_coinPropStr = (`_coinProp2Str` _coinPropMap)

_coinQPropStr :: CoinProperty -> Maybe String
_coinQPropStr = (`_coinProp2Str` _coinQPropMap)

_coinProp2Str :: CoinProperty -> CoinPropMap -> Maybe String
_coinProp2Str cp m= _cfsFieldName <$> M.lookup cp m

type CoinQuoteUnit = String -- USD, BTC, Eth, etc...
type CoinPropMap = Map CoinProperty CoinFieldSchema

data CoinStorageType = CstObject | CstArray | CstString | CstInteger | CstReal | CstBool | CstNull
  deriving (Eq,Ord,Enum,Show)

data CoinFieldRequired = CfOptional | CfRequired
  deriving (Eq,Ord,Enum,Show)

data CoinFieldSchema = CoinFieldSchema
  {
    _cfsFieldName :: String
  , _cfsStorage :: CoinStorageType
  , _cfsRequired :: CoinFieldRequired
  }

-- coin schema declarations - could be moved to a json config file

_coinPropMap :: CoinPropMap
_coinPropMap = M.fromList
  [
    (CoinId,                            CoinFieldSchema   "id"                                CstInteger  CfRequired)
  , (CoinName,                          CoinFieldSchema   "name"                              CstString   CfRequired)
  , (CoinSymbol,                        CoinFieldSchema   "symbol"                            CstString   CfRequired)
  , (CoinSlug,                          CoinFieldSchema   "slug"                              CstString   CfRequired)
  , (CoinCmcRank,                       CoinFieldSchema   "cmc_rank"                          CstInteger  CfRequired)
  , (CoinNumMarketPairs,                CoinFieldSchema   "num_market_pairs"                  CstInteger  CfRequired)
  , (CoinCirculatingSupply,             CoinFieldSchema   "circulating_supply"                CstReal     CfRequired)
  , (CoinTotalSupply,                   CoinFieldSchema   "total_supply"                      CstReal     CfRequired)
  , (CoinMaxSupply,                     CoinFieldSchema   "max_supply"                        CstReal     CfRequired)
  , (CoinLastUpdated,                   CoinFieldSchema   "last_updated"                      CstString   CfRequired)
  , (CoinDateAdded,                     CoinFieldSchema   "date_added"                        CstString   CfRequired)
  -- , (CoinTags,                          CoinFieldSchema   "tags"                              CstArray    CfRequired)
  , (CoinSelfReportedCirculatingSupply, CoinFieldSchema   "self_reported_circulating_supply"  CstReal     CfOptional)
  , (CoinSelfReportedMarketCap,         CoinFieldSchema   "self_reported_market_cap"          CstReal     CfOptional)
  ]

_coinQPropMap :: CoinPropMap
_coinQPropMap = M.fromList
  [
    (CoinQPrice,                        CoinFieldSchema   "price"                             CstReal    CfRequired)
  , (CoinQVolume24H,                    CoinFieldSchema   "volume_24h"                        CstReal    CfRequired)
  , (CoinQVolumeChange24h,              CoinFieldSchema   "volume_change_24h"                 CstReal    CfRequired)
  , (CoinQPercentChange1h,              CoinFieldSchema   "percent_change_1h"                 CstReal    CfRequired)
  , (CoinQPercentChange24h,             CoinFieldSchema   "percent_change_24h"                CstReal    CfRequired)
  , (CoinQPercentChange7d,              CoinFieldSchema   "percent_change_7d"                 CstReal    CfRequired)
  , (CoinQMarketCap,                    CoinFieldSchema   "market_cap"                        CstReal    CfRequired)
  , (CoinQMarketCapDominance,           CoinFieldSchema   "market_cap_dominance"              CstReal    CfRequired)
  , (CoinQFullyDilutedMarketCap,        CoinFieldSchema   "fully_diluted_market_cap"          CstReal    CfRequired)
  , (CoinQLastUpdated,                  CoinFieldSchema   "last_updated"                      CstString  CfRequired)
  ]

qq = QuasiQuoter { 
    quoteExp = stringE -- only interested in expressions currently
  , quotePat = undefined
  , quoteType = undefined
  , quoteDec = undefined
  }

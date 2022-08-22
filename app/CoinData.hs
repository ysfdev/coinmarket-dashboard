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

import Data.Map (Map)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Maybe
import Data.Aeson
import Data.Aeson.Types

import CoinDataUtils
import CoinDataTypes
import CoinDataSample
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


----- =============== End Pure Code =============== -----


-- Should perform full text search look up on local coin data store based on _coinSlug (coin name)
coinLookUp :: CoinLookupParams -> IO CoinLookupResult
-- coinLookUp _ = return ClrNotFoundError
coinLookUp _ = let  
  a = case decode (toBStr _sampleCoinListData) of 
    Just o -> parseMaybe (.:: "data") o :: Maybe (Vector Coin)
    _ -> Nothing in 
  case a of
    Just va ->
      if not $ null va then return $ ClrCoin $ Just $ V.head va
      else return ClrNotFoundError
    _ -> return ClrNotFoundError

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
    _ -> Nothing in
  case a of
    Just va -> return $ GcrCoinList $ V.take 10 va
    _ -> return GcrUnexpectedError
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}

module CoinData 
(
  Coin (..)
, CoinProperty (..)
, CoinLookupParams (..)
, CoinLookupResult (..)
, CoinTextSearchStyle (..)
, CoinLookupSearchData (..)
, GetCoinsResult (..)
, coinLookup
, top10Coins
) where

import Data.Map (Map)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Maybe
import Data.Aeson
import Data.Aeson.Types

import qualified Control.Concurrent.MVar as M

import Database.SQLite3(Database)

import CoinDataUtils
import CoinDataTypes
import CoinDataSample
import qualified CoinDataStorage as CDS
import qualified DataRefresherTypes as DR

----- =============== End Pure Code =============== -----

-- Should perform full text search look up on local coin data store based on _coinSlug (coin name)
coinLookup :: DR.SContext -> CoinLookupParams -> IO CoinLookupResult
coinLookup sCtx (CoinLookupParams sd ss ) = let
  (srchProp,sBaseStr) = case sd of
    ClsdName s -> (CoinName,s)
    ClsdSymbol s -> (CoinSymbol,s)
    ClsdSlug s -> (CoinSlug,s)
  srchStr = case ss of
    CtssBegin -> sBaseStr <> "%"
    CtssEnd -> "%" <> sBaseStr
    CtssContain -> "%" <> sBaseStr <> "%" in 
  M.takeMVar sCtx >>= \sc -> let -- start blocking db access
  db = DR.storageHandle sc in
  CDS.coinLookup db srchProp srchStr >>= \rslt ->
  M.putMVar sCtx sc >> -- end blocking db access
  return rslt

-- Fetch from DB
getCoins :: DR.SContext -> GetCoinsParams -> IO GetCoinsResult
getCoins sCtx (GetCoinsParams limit sortProp _ unit) =
  M.takeMVar sCtx >>= \sc -> let -- start blocking db access
  db = DR.storageHandle sc in
  CDS.fetchTopNCoins db unit limit sortProp >>= \rslt ->
  M.putMVar sCtx sc >> -- end blocking db access
  return rslt

-- Fetch the top ten coins from the local store
top10Coins :: DR.SContext -> IO GetCoinsResult
top10Coins sCtx = getCoins sCtx (GetCoinsParams {_gcpLimit=10, _gcpSortBy=CoinCmcRank, _gcpFilterBy=CoinId, _gcpUnit="USD"})
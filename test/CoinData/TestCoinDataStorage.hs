module TestCoinDataStorage where

import Debug.Trace

import CoinDataTypes
import CoinDataStorage
import CoinDataUtils hiding (trace)
import CoinDataStorageUtils hiding (trace)
import CoinDataSample

import MarketDataClient
import qualified MarketDataClientTypes as NT

import Control.Monad.IO.Class
import Control.Monad.State.Class
import Data.Aeson
import Data.Aeson.Types
import qualified Data.Aeson.Key as AK
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Map (Map)
import qualified Data.Map as M
import qualified Database.SQLite3.Direct as DBD
import qualified Database.SQLite3 as DB
import Data.Int

_dbLocation = "./test/CoinData/coin-test.db"

testInit = initializeDB _dbLocation

testInitAndInsert =
  initializeDB _dbLocation >>= \db ->
  case toArrayFromString _sampleCoinListData of
    Error err -> putStrLn err >> DB.close db
    Success vc -> insertCoins db vc >> DB.close db

testInitAndInsert1 =
  initializeDB _dbLocation >>= \db ->
  case V.take 1 <$> toArrayFromString _sampleCoinListData of
    Error err -> putStrLn err >> DB.close db
    Success vc -> insertCoins db vc >> DB.close db

testInitAndInsertMultipleQuotes =
  initializeDB _dbLocation >>= \db ->
  case V.fromList.(:[]) <$> (decode $ toBStr _sampleCoinData1 :: Maybe Coin)  of
    Nothing -> putStrLn "failed to pars coin" >> DB.close db
    Just vc -> insertCoins db vc >> DB.close db

testBuildInsertStatmentValList :: IO ()
testBuildInsertStatmentValList = 
  case toArrayFromString _sampleCoinListData of
    Error err -> putStrLn err
    Success vc -> mapM_ (\v -> putStr (show v <> "\n")) (_buildInsertStatementValList vc)

testBuilInsertQuoteStatementValList :: IO ()
testBuilInsertQuoteStatementValList =
  case toArrayFromString _sampleCoinListData of
    Error err -> putStrLn err
    Success vc -> mapM_ (\v -> putStr (show v <> "\n")) (snd._buildQuoteInsertStatmentValList $ vc)

testBuilInsertMultiQuoteCoinStatementValList :: IO ()
testBuilInsertMultiQuoteCoinStatementValList = 
  case (decode $ toBStr _sampleCoinData1 :: Maybe Coin) of
    Nothing -> putStrLn "ERROR: Failed to parse multiquote coin!"
    Just vc -> mapM_ (\v -> putStr (show v <> "\n")) (mconcat <$> snd (_buildQInsertRowValList 1 vc))

testFetchTopNCoins = let
  qUnit = "USD"
  limit = 10 
  sortProp = CoinId in 
  initializeDB _dbLocation >>= \db ->
  fetchTopNCoins db qUnit limit sortProp

testCoinLookup = let
  searchProp = CoinSymbol
  searchStr = "l%" in
  initializeDB _dbLocation >>= \db ->
  coinLookup db searchProp searchStr

testInsertCoins = 
  case toArrayFromString _sampleCoinListData of
    Error err -> putStrLn err >> return False
    Success vc -> let
      numCoins = V.length vc in
      initializeDB _dbLocation >>= \db ->
      insertCoins db vc >>
      DB.changes db >>= \numChanges ->
      DB.close db >>
      putStrLn (show numChanges <> " rows changed out of " <> show numCoins <> " requested") >>
      return (numChanges == numCoins)

_coinTestStatement sortCol = DBD.Utf8 $ toSBStr $
      "select "
  <>  _coinSQLColStr
  <>  ", "
  <>  _coinQSqlColStr
  <>  " from coindata inner join quotedata on coindata.id = quotedata.id where "
  <>  sortCol
  <>  " like :search order by cmc_rank;"

testPrepareStatement = let 
  searchProp = CoinSlug in
  case _getDBScopedPropName searchProp of
    Nothing -> print "Failed to find name for prop"
    Just searchCol ->
      initializeDB _dbLocation >>= \db ->
      --select * from coindata inner join quotedata on coindata.id = quotedata.id where symbol = "ETH" order by cmc_rank limit 10;
      trace "testStatement -- calling prepare" DB.prepareUtf8 db (_coinTestStatement searchCol) >>= \stmt ->
      trace "testStatement -- calling bind" DB.bindNamed stmt
        [
           (toText ":search",  toSQLText "Car%")
        ] >>
      trace "testStatement -- calling _processResults" _processResults stmt V.empty >>= \cs ->
      DB.finalize stmt >>
      DB.close db >>
      print (V.take 10 cs)

testDataClientIntegration :: IO Bool
testDataClientIntegration =let
  numCoins = 3000
  tdcParams = NT.QueryParams 0 numCoins in
  fetchLatestListings tdcParams >>= \rsp -> let
  status = NT.status rsp
  msg = NT.message rsp
  body = NT.body rsp
  rCoins = V.mapM (parse parseJSON) body in
  initializeDB _dbLocation >>= \db ->
  case rCoins of
    Error err -> putStrLn err >> DB.close db >> return False
    Success coins ->
      insertCoins db coins >>
      DB.changes db >>= \numChanges ->
      DB.close db >>
      putStrLn (show numChanges <> " rows changed out of " <> show numCoins <> " requested") >>
      return (numChanges == numCoins)

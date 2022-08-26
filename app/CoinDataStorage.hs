module CoinDataStorage where

-- Control Imports --
import Control.Exception

-- CoinData imports -- 
import CoinDataTypes
import CoinDataUtils
import CoinDataStorageUtils hiding (trace)

-- import Debug.Trace

-- DB imports -- 
import qualified Database.SQLite3 as DB
import qualified Database.SQLite3.Direct as DBD

-- Data imports --
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Map as M


initializeDB :: String -> IO DB.Database
initializeDB dbLoc = DB.open (toText dbLoc) >>= \db ->
  DB.exec db _initDB >>
  return db

insertCoins :: DB.Database -> Vector Coin -> IO ()
insertCoins db vc = catch(_exInsertCoins db vc) handleIt where
  handleIt e =
    case fromException e of
      Just (DB.SQLError code details context) -> 
        case (code,_matchErr _missingColumnErr details) of 
          (DB.ErrorError,idx:_) -> -- attempt to handle schema changes
            let sndHandler (SomeException e) = throwIO (CoinDataStorageError _insertFailureMsg) in
            catch (__clearAndInit db) sndHandler >> -- attempt to drop tables and reinit db
            catch (_exInsertCoins db vc) sndHandler -- try inserting one more time...
          _ -> throwIO (CoinDataStorageError _insertFailureMsg)
      _-> throwIO e

_exInsertCoins :: DB.Database -> Vector Coin -> IO ()
_exInsertCoins db vc = let 
  numCoins = V.length vc 
  coinList = _buildInsertStatementValList vc
  (numQuotes,qList) = _buildQuoteInsertStatmentValList vc in
  DB.prepareUtf8 db (_coinInsertStatement numCoins) >>= \stmt ->
  DB.bindNamed stmt coinList >>
  _processInsertResults stmt >>
  DB.finalize stmt >>
  DB.prepareUtf8 db (_coinQInsertStatement numQuotes) >>= \stmt ->
  DB.bindNamed stmt qList >>
  _processInsertResults stmt >>
  DB.finalize stmt

fetchTopNCoins :: DB.Database -> String -> Int -> CoinProperty -> IO GetCoinsResult
fetchTopNCoins db qUnit limit sortProp = catch (_exFetchTopNCoins db qUnit limit sortProp) handleIt where
  handleIt e = 
    case fromException e of
      Just (DB.SQLError code details context) -> return GcrUnexpectedError
      _-> throwIO e

_exFetchTopNCoins :: DB.Database -> String -> Int -> CoinProperty -> IO GetCoinsResult
_exFetchTopNCoins db qUnit limit sortProp =
  case _getDBScopedPropName sortProp of
    Nothing -> return GcrUnexpectedError
    Just sortCol ->
      DB.prepareUtf8 db (_coinTopNStatement' sortCol) >>= \stmt ->
      DB.bindNamed stmt
        [
          (toText ":unit",    toSQLText qUnit)
        , (toText ":limit",    toSQLInteger (fromIntegral limit))
        ] >>
      _processResults stmt V.empty >>= \cs ->
      DB.finalize stmt >>
      return (GcrCoinList cs)

-- For partial matches searchStr should have % prepended, appended,
-- or both for ends with, begins with, and cointains style searches
-- ASCII based searches are NOT case sensitive while
-- multi-byte unicode character based searches ARE case sensitive
-- Ex:
-- Find coins that cointain CD in their symbol
-- searchProp: CoinSymbol
-- searchStr: "%BT%"
--  
coinLookup :: DB.Database -> CoinProperty -> String -> IO CoinLookupResult
coinLookup db searchProp searchStr = catch (_exCoinLookup db searchProp searchStr) handleIt where
      handleIt e = 
        case fromException e of
          Just (DB.SQLError code details context) -> return ClrUnexpectedError
          _-> throwIO e

_exCoinLookup :: DB.Database -> CoinProperty -> String -> IO CoinLookupResult
_exCoinLookup db searchProp searchStr =
  case _getDBScopedPropName searchProp of
    Nothing -> return ClrUnexpectedError
    Just searchCol ->
      DB.prepareUtf8 db (_coinLookupStatement' searchCol)>>= \stmt ->
      DB.bindNamed stmt
        [
          (toText ":search",  toSQLText searchStr)
        ] >>
      _processResults stmt V.empty >>= \cs ->
      DB.finalize stmt >>
      if null cs then return ClrNotFoundError
      else return (ClrCoin (V.head cs))

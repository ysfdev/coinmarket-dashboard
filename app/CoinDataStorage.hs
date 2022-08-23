module CoinDataStorage where

-- Control Imports --
import Control.Exception

-- CoinData imports -- 
import CoinDataTypes
import CoinDataUtils
import CoinDataStorageUtils

-- DB imports -- 
import qualified Database.SQLite3 as DB
import qualified Database.SQLite3.Direct as DBD

-- Data imports --
import qualified Data.Vector as V
import qualified Data.Map as M

-- displayException :: SQLError -> String
errorHandler ex =
  putStrLn $ "Caught exception: " ++ show ex

-- catch (print $ 5 `div` 0) handler
initializeDB :: String -> IO DB.Database
initializeDB dbLoc = DB.open (toText dbLoc) >>= \db ->
  DB.exec db _initDB >>
  return db

insertCoins :: Foldable t => DB.Database -> t Coin -> IO ()
insertCoins db vc = DB.exec db $ _buildInsertStatemnt vc

fetchTopNCoins :: DB.Database -> String -> Int -> CoinProperty -> IO GetCoinsResult
fetchTopNCoins db qUnit limit sortProp =
  case _getPropName sortProp of
    Nothing -> return GcrUnexpectedError
    Just sortCol ->
      DB.prepareUtf8 db (_coinTopNStatement sortCol) >>= \stmt ->
      DB.bindNamed stmt
        [
          (toText ":unit",    toSQLText qUnit)
        ] >>
      _processResults stmt M.empty >>= \cs ->
      DB.finalize stmt >>
      return (GcrCoinList $ V.fromList $ take limit (M.elems cs))

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
coinLookup db searchProp searchStr =
  case _getPropName searchProp of
    Nothing -> return ClrUnexpectedError
    Just searchCol ->
      DB.prepareUtf8 db (_coinLookupStatement searchCol)>>= \stmt ->
      DB.bindNamed stmt
        [
          (toText ":search",  toSQLText searchStr)
        ] >>
      _processResults stmt M.empty >>= \cs ->
      DB.finalize stmt >>
      if null cs then return ClrNotFoundError
      else return (ClrCoin (head $ M.elems cs))
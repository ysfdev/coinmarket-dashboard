{-# LANGUAGE QuasiQuotes #-}

module CoinDataStorageUtils where

import CoinDataTypes
import CoinDataUtils

import qualified Database.SQLite3 as DB
import qualified Database.SQLite3.Direct as DBD
import Control.Monad.IO.Class
import Control.Monad.State.Class
import Control.Exception

--- Data imports ---

import Data.Int
import qualified Data.Scientific as SC
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Internal.Lazy.Search as TLS
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Map (Map)
import qualified Data.Map as M
import Data.Aeson
import Data.Aeson.Types

-- import Debug.Trace
-- disable debug tracing
trace _ a = a

toSQLText = DB.SQLText . toText
toSQLInteger = DB.SQLInteger

coinStorageTypeToSQLType :: CoinStorageType -> DB.ColumnType
coinStorageTypeToSQLType cs = case cs of
  CstObject -> DB.BlobColumn
  CstArray -> DB.BlobColumn
  CstString -> DB.TextColumn
  CstInteger -> DB.IntegerColumn
  CstReal -> DB.FloatColumn
  CstBool -> DB.IntegerColumn
  CstNull -> DB.NullColumn

fromSQLInteger :: DB.SQLData -> Maybe Int64
fromSQLInteger  v = case v of DB.SQLInteger  i -> Just i;_->Nothing
fromSQLFloat :: DB.SQLData -> Maybe Double
fromSQLFloat    v = case v of DB.SQLFloat    f -> Just f;_->Nothing
fromSQLText :: DB.SQLData -> Maybe Text
fromSQLText     v = case v of DB.SQLText     s -> Just s;_->Nothing
fromSQLBlob :: DB.SQLData -> Maybe BS.ByteString
fromSQLBlob     v = case v of DB.SQLBlob     bs -> Just bs;_->Nothing

coinPropValSQLData :: Object -> CoinFieldSchema -> Result DB.SQLData
coinPropValSQLData o schema =
    case parse (.:: _cfsFieldName schema) o :: Result Value of
    Error err -> case _cfsRequired schema of
      CfRequired -> Error err
      CfOptional -> Success DB.SQLNull -- return "null" for optional fields
    Success v -> case v of
      Object o -> Success $ DB.SQLBlob . toSBStr.fromBStr $ encode o
      Array a -> Success $ DB.SQLBlob . toSBStr.fromBStr $ encode a
      String s -> Success $ DB.SQLText s
      Number n -> Success $
        if _cfsStorage schema == CstInteger then DB.SQLInteger (floor n) -- make sure int types have no decimals
        else DB.SQLFloat $ SC.toRealFloat n
      Bool b -> Success $ DB.SQLInteger $ if b then 1; else 0
      Null -> Success DB.SQLNull

_getDBScopedPropName :: CoinProperty -> Maybe String
_getDBScopedPropName prop =
  case M.lookup prop _coinPropMap of
    Just schema -> Just $ "coindata." <> _cfsFieldName schema
    Nothing -> case M.lookup prop _coinQPropMap of
      Nothing -> Nothing
      Just schema -> Just $ "quotedata." <> _cfsFieldName schema

--- for debugging ---
translateErrors :: DB.Error -> String
translateErrors e = case e of
  DB.ErrorOK                  -> "Successful result"
  DB.ErrorError               -> "SQL error or missing database"
  DB.ErrorInternal            -> "Internal logic error in SQLite"
  DB.ErrorPermission          -> "Access permission denied"
  DB.ErrorAbort               -> "Callback routine requested an abort"
  DB.ErrorBusy                -> "The database file is locked"
  DB.ErrorLocked              -> "A table in the database is locked"
  DB.ErrorNoMemory            -> "A malloc() failed"
  DB.ErrorReadOnly            -> "Attempt to write a readonly database"
  DB.ErrorInterrupt           -> "Operation terminated by sqlite3_interrupt()"
  DB.ErrorIO                  -> "Some kind of disk I/O error occurred"
  DB.ErrorCorrupt             -> "The database disk image is malformed"
  DB.ErrorNotFound            -> "Unknown opcode in sqlite3_file_control()"
  DB.ErrorFull                -> "Insertion failed because database is full"
  DB.ErrorCan'tOpen           -> "Unable to open the database file"
  DB.ErrorProtocol            -> "Database lock protocol error"
  DB.ErrorEmpty               -> "Database is empty"
  DB.ErrorSchema              -> "The database schema changed"
  DB.ErrorTooBig              -> "String or BLOB exceeds size limit"
  DB.ErrorConstraint          -> "Abort due to constraint violation"
  DB.ErrorMismatch            -> "Data type mismatch"
  DB.ErrorMisuse              -> "Library used incorrectly"
  DB.ErrorNoLargeFileSupport  -> "Uses OS features not supported on host"
  DB.ErrorAuthorization       -> "Authorization denied"
  DB.ErrorFormat              -> "Auxiliary database format error"
  DB.ErrorRange               -> "2nd parameter to sqlite3_bind out of range"
  DB.ErrorNotADatabase        -> "File opened that is not a database file"
  DB.ErrorNotice              -> "Notifications from sqlite3_log()"
  DB.ErrorWarning             -> "Warnings from sqlite3_log()"
  DB.ErrorRow                 -> "sqlite3_step() has another row ready"
  DB.ErrorDone                -> "sqlite3_step() has finished executing"

_missingColumnErr = TL.pack "has no column named"
_matchErr pattern errStr = TLS.indices pattern (TL.pack $ T.unpack errStr)
_insertFailureMsg = TL.pack "Failed to insert coins. Try deleting the database and restarting the app."

newtype CoinDataStorageError = CoinDataStorageError {
  cdsErrorMsg :: TL.Text
}

instance Show CoinDataStorageError where
  show (CoinDataStorageError err) = "CoinDataStorageError -- " <> show err

instance Exception CoinDataStorageError

_createCoinDataTable = [qq|
create table if not exists coindata
(
 id integer primary key
,name text not null
,symbol text not null
,slug text not null
,cmc_rank integer not null
,num_market_pairs integer
,circulating_supply real
,total_supply real
,max_supply real
,last_updated text
,date_added text
,tags blob
,self_reported_circulating_supply real
,self_reported_market_cap real
,quote blob
);
|]

_createCoinDataIndexes = [qq|
create index if not exists idx_coinname on coindata (name);
create index if not exists idx_coinsymbol on coindata (symbol);
create index if not exists idx_coinslug on coindata (slug);
create index if not exists idx_coinlastupdated on coindata (last_updated);
create unique index if not exists idx_coinrank on coindata (cmc_rank);
|]

_createQuoteDataTable = [qq|
create table if not exists quotedata
(
 price real
,volume_24h real
,volume_change_24h real
,percent_change_1h real
,percent_change_24h real
,percent_change_7d real
,market_cap real
,market_cap_dominance real
,fully_diluted_market_cap real
,last_updated text
,id integer not null
,unit text not null
,primary key (id, unit)
,foreign key (id) references coindata (id) on delete cascade on update cascade
);
|]

_createQuoteDataIndexes = [qq|
create index if not exists idx_quotecoin on quotedata (id);
create index if not exists idx_quotelastupdated on quotedata (unit,last_updated);
create index if not exists idx_quotecoinprice on quotedata (unit,price);
create index if not exists idx_quotemc on quotedata (unit,market_cap);
|]

_createIndexes = _createCoinDataIndexes <> _createQuoteDataIndexes

_createTables = _createCoinDataTable <> _createQuoteDataTable

_dropTables = [qq|
drop table if exists coindata;
drop table if exists quotedata;
|]

_initPrologue = [qq|
/* Database initialization Prologue */
pragma foreign_keys = ON; /*turn on foreign key constraints*/
|]

_initBody :: [Char]
_initBody = "/* Database initialization Prologue */\n"
  -- <> _dropTables
  <> _createTables
  <> _createIndexes

_initEpillogue = [qq|
/*Database initialization epilogue*/
|]

_initDB = toText $ _initPrologue <> _initBody <> _initEpillogue

__dropTables db = DB.exec db (toText _dropTables)

__clearAndInit db =
  __dropTables db >>
  DB.exec db _initDB

(<+>) :: [Char] -> [Char] -> [Char]
(<+>) = sqlStmtRowAppend -- append a new row to a sql statment
sqlStmtRowAppend :: [Char] -> [Char] -> [Char]
sqlStmtRowAppend a b = case a of "" -> b;_ -> a <> ",\n" <> b

_coinQUnitStr = "unit"
_coinQuoteStr = _cfsFieldName _coinQuoteSchema
_coinQuoteKey = toKey _coinQuoteStr
_coinIdStr = case _cfsFieldName <$> M.lookup CoinId _coinPropMap of Just s -> s; _ -> error "Fatal Error: No string defined for coin id property! Check the definition of _coinPropMap"
_coinIdKey = toKey _coinIdStr
_coinCols = map _cfsFieldName $ M.elems _coinPropMap
_coinColStr = mconcat ["(",_getColStr _coinCols,")"]
_coinQCols = map _cfsFieldName $ M.elems _coinQPropMap
_coinQColStr = mconcat ["(",_getColStr _coinQCols,",",_coinIdStr,",",_coinQUnitStr,")"]
_getColStr = foldl1 (\acc s-> acc <> "," <> s)


--- SQL Coin Column Info ---
_coinSQLSchema = M.elems _coinPropMap
_coinSQLColStr = foldl1 (\acc col->acc<>", "<>col) (map (\sc->"coindata."<>_cfsFieldName sc) _coinSQLSchema)
_coinSQLColTypes = map (Just . coinStorageTypeToSQLType._cfsStorage) _coinSQLSchema
_coinNumSQLCols = length _coinSQLColTypes
_coinColNamedParamsList = map (\sc->":"<>_cfsFieldName sc) _coinSQLSchema


-- SQL Coin Insert Functions --
_coinInsertHeader = "insert or replace into coindata "
  <> _coinColStr
  <> "\nvalues\n"

_coinQInsertHeader = "insert or replace into quotedata "
  <> _coinQColStr
  <> "\nvalues\n"

_buildInsertRowValList :: Int -> Coin -> Result [(Text, DB.SQLData)]
_buildInsertRowValList idx c = mapM (\schm -> 
  let rvp = coinPropValSQLData (_coinJSON c) schm in
    case rvp of
      Error err -> Error err 
      Success vp -> Success (T.pack $ ":"<>_cfsFieldName schm<>"_"<>show idx,vp)) $ M.elems _coinPropMap

_buildInsertStatementValList :: Vector Coin -> [(Text, DB.SQLData)]
_buildInsertStatementValList vc = snd $
  foldl (\(idx,acc) c -> case _buildInsertRowValList idx c of
      Error err -> trace ("_buildInsertStatementValList -- skipping insert value row -- " <> err) (idx,acc)
      Success tpv -> (idx+1,acc<>tpv)) (1,[]) vc

_coinInsertStatement :: Int -> DBD.Utf8
_coinInsertStatement n = DBD.Utf8 $ toSBStr $
      _coinInsertHeader
  <>  _buildInsertStatementNamedParamList n
  <>  ";"

--- SQL Quote Column Info ---
_coinQSQLSchema = M.elems _coinQPropMap <>
  [
    CoinFieldSchema _coinIdStr    CstInteger CfRequired
  , CoinFieldSchema _coinQUnitStr CstString  CfRequired
  ]
_coinQSqlColStr = foldl1 (\acc col->acc<>", "<>col) (map (\sc->"quotedata."<>_cfsFieldName sc) _coinQSQLSchema)
_coinQSQLColTypes = map (Just . coinStorageTypeToSQLType._cfsStorage) _coinQSQLSchema
_coinQNumSQLCols = length _coinQSQLColTypes
_coinQColNamedParamsList = map (\sc->":"<>_cfsFieldName sc) _coinQSQLSchema

_buildInsertQuoteStatementNamedParamList :: Int -> String
_buildInsertQuoteStatementNamedParamList n = if n > 0 then
  snd (foldl (\(idx,acc) cs->(idx+1,acc <> ",\n" <> "(" <> _buildNamedParamRow (idx,cs) <> ")")) 
  (2,"(" <> _buildNamedParamRow (1,_coinQColNamedParamsList) <> ")")
  (replicate (n-1) _coinQColNamedParamsList))
  else ""

_buildQInsertRowValList :: Int -> Coin -> (Int, Result [[(Text, DB.SQLData)]])
_buildQInsertRowValList idx0 c = let
  qm = zip [idx0..] (M.toList (_quoteJSON c))
  lastIdx = fst.last $ qm
  rowList = mapM (\(idx,(unit,quote)) -> _coinId c >>= \cid -> let 
    runit = return [(toText (":"<>_coinQUnitStr<>"_"<>show idx), toSQLText unit)]
    rcid = return [(toText (":"<>_coinIdStr<>"_"<>show idx), toSQLInteger.fromIntegral $ cid)] in
    mapM (\schm ->
      case coinPropValSQLData quote schm of
        Error err -> Error err 
        Success vp -> Success (T.pack $ ":"<>_cfsFieldName schm<>"_"<>show idx,vp)) 
    (M.elems _coinQPropMap) <:> rcid <:> runit) qm in (lastIdx,rowList)

_buildQuoteInsertStatmentValList :: Vector Coin -> (Int,[(Text, DB.SQLData)])
_buildQuoteInsertStatmentValList = foldl (\(idx,acc) c -> case _buildQInsertRowValList (idx+1) c of
      (_,Error err) -> trace ("_buildQuoteInsertStatmentValList -- skipping insert value row -- " <> err) (idx,acc)
      (nIdx, Success tpv) -> {-trace ("quotes processed: " <> show nIdx)-} (nIdx,acc<>mconcat tpv)) (0,[])

_coinQInsertStatement :: Int -> DBD.Utf8
_coinQInsertStatement n = DBD.Utf8 $ toSBStr $
      _coinQInsertHeader
  <>  _buildInsertQuoteStatementNamedParamList n
  <>  ";"

_buildNamedParamRow :: (Int,[String]) -> String
_buildNamedParamRow (_,[]) = ""
_buildNamedParamRow (idx,c1:cs) = foldl (\acc col->acc <> ", " <> col <> "_" <> show idx) (c1 <> "_" <> show idx) cs

_buildInsertStatementNamedParamList :: Int -> String
_buildInsertStatementNamedParamList n = if n > 0 then
  snd (foldl (\(idx,acc) cs->(idx+1,acc <> ",\n" <> "(" <> _buildNamedParamRow (idx,cs) <> ")")) 
  (2,"(" <> _buildNamedParamRow (1,_coinColNamedParamsList) <> ")")
  (replicate (n-1) _coinColNamedParamsList))
  else ""

--- SQL Result handling functions ---

_coinTopNStatement :: String -> DBD.Utf8
_coinTopNStatement sortCol = DBD.Utf8 $ toSBStr $
      "select "
  <>  _coinSQLColStr
  <>  ", "
  <>  _coinQSqlColStr
  <>  " from coindata inner join quotedata on coindata.id = quotedata.id and quotedata.unit = :unit order by "
  <>  sortCol
  <>  ";"

_coinTopNStatement' :: String -> DBD.Utf8
_coinTopNStatement' sortCol = DBD.Utf8 $ toSBStr $
      "select "
  <>  _coinSQLColStr
  <>  " from coindata inner join quotedata on coindata.id = quotedata.id and quotedata.unit = :unit order by "
  <>  sortCol
  <>  " limit :limit;"

_coinLookupStatement :: String -> DBD.Utf8
_coinLookupStatement searchCol = DBD.Utf8 $ toSBStr $
      "select "
  <>  _coinSQLColStr
  <>  ", "
  <>  _coinQSqlColStr
  <>  " from coindata where "
  <>  searchCol
  <>  " like :search order by cmc_rank;"

_coinLookupStatement' :: String -> DBD.Utf8
_coinLookupStatement' searchCol = DBD.Utf8 $ toSBStr $
      "select "
  <>  _coinSQLColStr
  <>  " from coindata where "
  <>  searchCol
  <>  " like :search order by cmc_rank;"

_nullOnOptional val req k =
  if req == CfRequired then val
  else val <> [k.=Null]

_getRowObject :: [Pair] -> (DB.SQLData,CoinFieldSchema) -> [Pair]
_getRowObject kvs (sqldata,schema) = let
  propKey = toKey $ _cfsFieldName schema
  propType = _cfsStorage schema
  propReq = _cfsRequired schema
  in case sqldata of
    DB.SQLInteger int ->
      if propType == CstInteger then kvs <> [propKey .= int]
      else _nullOnOptional kvs propReq propKey
    DB.SQLFloat dbl ->
      if propType == CstReal then kvs <> [propKey .= dbl]
      else _nullOnOptional kvs propReq propKey
    DB.SQLText txt ->
      if propType == CstString then kvs <> [propKey .= T.unpack txt]
      else _nullOnOptional kvs propReq propKey
    DB.SQLBlob bs -> case propType of
      CstObject -> case decode (toBStr . fromSBStr $ bs) :: Maybe Object of
        Nothing -> _nullOnOptional kvs propReq propKey
        Just o -> kvs <> [propKey .= o]
      CstArray ->case decode (toBStr . fromSBStr $ bs) :: Maybe Array of
        Nothing -> _nullOnOptional kvs propReq propKey
        Just a -> kvs <> [propKey .= a]
      _-> _nullOnOptional kvs propReq propKey
    DB.SQLNull -> kvs <> [propKey.=Null]

_getNextCol :: DB.Statement -> Vector Coin -> IO (Vector Coin)
_getNextCol stmt cs0 =
  DB.typedColumns stmt _coinSQLColTypes >>= \sqlData -> let
  cColInfo = zip sqlData _coinSQLSchema
  cs1 = let 
      kvs = foldl _getRowObject [] cColInfo 
      mc = parse parseJSON (object kvs) :: Result Coin in
      case mc of 
        Error err -> trace ("skip this coin -- " <> err) cs0 -- skip this coin 
        Success coin -> cs0 <> V.fromList [coin]
  in return cs1
      
_processResults :: DB.Statement -> Vector Coin -> IO (Vector Coin)
_processResults stmt cs0 =
  DB.step stmt >>= \rslt ->
  if rslt == DB.Row then
    _getNextCol stmt cs0 >>= \cs1 ->
    _processResults stmt cs1
  else return cs0

_processInsertResults :: DB.Statement -> IO Bool
_processInsertResults stmt =
  DB.step stmt >>= \rslt ->
  if rslt == DB.Done then return True
  else return False
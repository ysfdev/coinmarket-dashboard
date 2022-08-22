{-# LANGUAGE QuasiQuotes #-}

module CoinDataStorageUtils where

import CoinDataTypes
import CoinDataUtils

import qualified Database.SQLite3 as DB
import qualified Database.SQLite3.Direct as DBD
import Control.Monad.IO.Class
import Control.Monad.State.Class

--- Data imports ---

import Data.Int
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Aeson
import Data.Aeson.Types

-- import Debug.Trace
-- disable debug tracing
trace _ a = a

toSQLText = DB.SQLText . toText

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


_createCoinDataTable = [qq|
create table if not exists coindata
(id integer primary key
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
,self_reported_circulating_supply real
,self_reported_market_cap real
,tags text);
|]

_createCoinDataIndexes = [qq|
create index if not exists idx_coinname on coindata (name);
create index if not exists idx_coinsymbol on coindata (symbol);
create index if not exists idx_coinslug on coindata (slug);
create index if not exists idx_coinlastupdated on quotedata (last_updated);
create unique index if not exists idx_coinrank on coindata (cmc_rank);
|]

_createQuoteDataTable = [qq|
create table if not exists quotedata
(price real
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
,foreign key (id) references coindata (id) on delete cascade on update cascade);
|]

_createQuoteDataIndexes = [qq|
create index if not exists idx_quotecoin on quotedata (id);
create index if not exists idx_quotelastupdated on quotedata (unit,last_updated);
create index if not exists idx_quotecoinprice on quotedata (unit,price);
create index if not exists idx_quotemc on quotedata (unit,market_cap);
|]

_createIndexes = _createCoinDataIndexes <> _createQuoteDataIndexes

_createTables = _createCoinDataTable <> _createQuoteDataTable

_clearTables = [qq|
delete from coindata;
delete from quotedata;
|]

_initPrologue = [qq|
/* Database initialization Prologue */
pragma foreign_keys = ON; /*turn on foreign key constraints*/
|]

_initBody :: [Char]
_initBody = "/* Database initialization Prologue */\n"
  <> _createTables
  <> _createIndexes

_initEpillogue = [qq|
/*Database initialization epilogue*/
|]

_initDB = toText $ _initPrologue <> _initBody <> _initEpillogue

-- data CoinInsert = CoinStorage

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


--- SQL Quote Column Info ---
_coinQSQLSchema = M.elems _coinQPropMap <>
  [
    CoinFieldSchema _coinIdStr    CstInteger CfRequired
  , CoinFieldSchema _coinQUnitStr CstString  CfRequired
  ]
_coinQSqlColStr = foldl1 (\acc col->acc<>", "<>col) (map (\sc->"quotedata."<>_cfsFieldName sc) _coinQSQLSchema)
_coinQSQLColTypes = map (Just . coinStorageTypeToSQLType._cfsStorage) _coinQSQLSchema
_coinQNumSQLCols = length _coinQSQLColTypes

-- mapM function to get values, over json keys
_getCoinRowValList :: Coin -> Result [String]
_getCoinRowValList c = mapM (__coinPropValStr $ _coinJSON c) $ M.elems _coinPropMap

-- create a string containing the list of quote rows associated with the given coin id
-- quotes that fail to parse are dropped
_getCoinQRowValStr :: Map String Object -> [Char] -> [Char]
_getCoinQRowValStr qm cid  = let 
  qList = M.toList qm
  makeQRowStrList obj = mapM (__coinPropValStr obj) $ M.elems _coinQPropMap 
  makeQRowStr (unit,obj) = let
    s0 = foldl1 (\acc vstr -> acc <> "," <> vstr) <$> makeQRowStrList obj
    s1 = mappend "(" <$> s0 in
    (`mappend` ("," <> cid <> "," <> show unit <>")")) <$> s1
  foldRows rows (unit, obj) =
    case makeQRowStr (unit,obj) of
      Error err -> trace ("error: " ++ err ++ ", parsing quote: " ++ unit ++ "-- for coin id: " ++ show cid) rows -- drop rows for objects with failed parse attempts
      Success r -> rows<+>r
  in foldl foldRows "" qList

-- foldl1 function to get val list, and mapM strings to list of value row strings 
_getCoinRowValStr :: Coin -> Result String
_getCoinRowValStr c = let
  s0 = foldl1 (\acc rs -> acc <> "," <> rs) <$> _getCoinRowValList c 
  s1 = mappend "(" <$> s0 in
  (`mappend` ")") <$> s1

-- function for foldr - takes a coin and a tuple of string vectors, one for coins and the other for associated quotes
_processCoinsForInsert c (cs, qs) =
  case _getCoinRowValStr c of
    Error err -> (cs,qs) -- skip coin
    Success cr ->
      case _coinId c of
        Error err -> trace ("error: " ++ err ++ ", while getting coin id") (cs,qs) -- skip coin
        Success cid ->
          case _coinQuoteMap c of
            Error err -> trace ("error: " ++ err ++ " getting quote map -- for coin id: " ++ show cid) (cs,qs) -- skip coin
            Success qm ->
              case _getCoinQRowValStr qm (show cid) of
                "" -> trace ("empty quote data -- for coin id: " ++ show cid) (cs,qs) -- skip coin - can't use coins with no quote data
                qr -> (cs <+> cr, qs <+> qr)

_buildInsertStatemnt vc = let
  (cs,qs) = foldr _processCoinsForInsert ("","") vc 
  transactionHeader = "begin transaction;\n"
  coinStatment = _coinInsertHeader <> cs <> ";\n"
  coinQStatement = _coinQInsertHeader <> qs <> ";\n" 
  transactionFooter = "commit;\n" in
  toText $
  transactionHeader
  <> coinStatment
  <> coinQStatement
  <> transactionFooter

_coinTop10Statement :: Text
_coinTop10Statement = toText "select * from coindata inner join quotedata on coindata.id = quotedata.id and quotedata.unit = \"USD\" order by cmc_rank limit 10;"

_coinTopNStatement :: String -> DBD.Utf8
_coinTopNStatement sortCol = DBD.Utf8 $ toSBStr $
      "select "
  <>  _coinSQLColStr
  <>  ", "
  <>  _coinQSqlColStr
  <>  " from coindata inner join quotedata on coindata.id = quotedata.id and quotedata.unit = :unit order by "
  <>  sortCol
  <>  ";"

_coinLookupStatement :: String -> DBD.Utf8
_coinLookupStatement searchCol = DBD.Utf8 $ toSBStr $
      "select "
  <>  _coinSQLColStr
  <>  ", "
  <>  _coinQSqlColStr
  <>  " from coindata inner join quotedata on coindata.id = quotedata.id where "
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

_getNextCol :: DB.Statement -> Map Int64 Coin -> IO (Map Int64 Coin)
_getNextCol stmt cs0 = let
  colTypes = _coinSQLColTypes <> _coinQSQLColTypes 
  schemaList = _coinSQLSchema <> _coinQSQLSchema in
  DB.typedColumns stmt colTypes >>= \sqlData -> let
  (cCols, qCols) = splitAt _coinNumSQLCols sqlData 
  (cColInfo,qColInfo) = 
    (
      zip cCols _coinSQLSchema
    , zip qCols _coinQSQLSchema
    )
  mcid = fromSQLInteger (head cCols)
  in
  case mcid of
    Nothing -> trace ("no coin id - skip it") return cs0 -- no coin id - skip it
    Just cid -> let
      cs1 = case M.lookup cid cs0 of
        Nothing -> let 
          x = foldl _getRowObject [] cColInfo 
          y = x <> [_coinQuoteKey .= object []]
          z = parse parseJSON (object y) :: Result Coin in
          case z of 
            Error err -> trace ("skip this coin -- " <> err) cs0 -- skip this coin 
            Success coin -> M.insert cid coin cs0
        Just v -> cs0
      cs2 = 
        if null cs1 then M.empty
        else let 
          x = foldl _getRowObject [] qColInfo 
          y = parse parseJSON (object x) :: Result Object 
          z = M.lookup cid cs1
          in case (y,z) of
            (Success q1,Just c0) ->
              case __coinPropVal q1 _coinQUnitStr :: Result String of
                Error err -> trace ("failed to get unit for quote -- " <> err) cs1 -- skip this coin
                Success unit -> let
                  qm0 = _quoteJSON c0
                  qm1 =
                    if null qm0 then M.fromList [(unit,q1)]
                    else M.insert unit q1 qm0
                  c1 = c0 {_quoteJSON = qm1}
                  in M.insert cid c1 cs1
            _ -> trace ("skip the quote - we must've skipped the coin above") cs1 -- skip the quote - we must've skipped the coin above
      in return cs2
      
_processResults :: DB.Statement -> Map Int64 Coin -> IO (Map Int64 Coin)
_processResults stmt cs0 =
  DB.step stmt >>= \rslt -> 
  if rslt == DB.Row then
    _getNextCol stmt cs0 >>= \cs1 ->
    _processResults stmt cs1
  else return cs0

_coinInsertHeader = "insert or replace into coindata "
  <> _coinColStr
  <> "\nvalues\n"

_coinQInsertHeader = "insert or replace into quotedata "
  <> _coinQColStr
  <> "\nvalues\n"
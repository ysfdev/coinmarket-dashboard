-- import Database.Sqlite
-- import Data.Text

{-# LANGUAGE
  QuasiQuotes
, TypeApplications
#-}

module TestCoinDataStorage where

import CoinDataUtils

_createCoinDataTable = [qq|
create table if not exists coindata
(id integer primary key
,name text not null
,symbol text not null
,slug text not null
,cmc_rank integer not null
,num_market_pairs integer
,circulating_supply integer
,total_supply integer
,max_supply integer
,last_updated text
,date_added text
,self_reported_circulating_supply integer
,self_reported_market_cap integer);
|]


_createQuoteDataTable = [qq|
create table if not exists quotedata
(price integer
,volume_24h integer
,volume_change_24h double
,percent_change_1h double
,percent_change_24h double
,percent_change_7d double
,market_cap double
,market_cap_dominance double
,fully_diluted_market_cap double
,last_updated text
,id integer not null
,unit text not null
,foreign key (id) references coindata (id));
|]

_initializeDatabaseScript = "begin transaction;"
-- <> _createCoinDataTable
-- <> _createQuoteDataTable ++ "commit;"


{-
PRAGMA foreign_keys=off;

-- start a new transaction. It ensures that all subsequent statements execute successfully or nothing executes at all.
BEGIN TRANSACTION;

ALTER TABLE table RENAME TO old_table;

-- define the primary key constraint here
CREATE TABLE table ( ... );
j[]
INSERT INTO table SELECT * FROM old_table;

-- commit all statments in the transaction
COMMIT;

PRAGMA foreign_keys=on;
-}

dbLocation = "./coin-test.db"

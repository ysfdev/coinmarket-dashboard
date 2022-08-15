-- import Database.Sqlite
-- import Data.Text

{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

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

_createCoinDataIndexes = [qq|
create index idx_coinname on coindata (name);
create index idx_coinsymbol on coindata (symbol);
create index idx_coinslug on coindata (slug);
create index idx_coincsupply on coindata (circulating_supply);
create index idx_cointotalsupply on coindata (total_supply);
create index idx_coinmaxsupply on coindata (max_supply);
create index idx_coinlastupdated on coindata (last_updated);
create unique index idx_coinrank on coindata (cmc_rank);
|]

_createQuoteDataTable = [qq|
create table if not exists quotedata
(price double
,volume_24h double
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
,primary key (id, unit)
,foreign key (id) references coindata (id) on delete cascade on update cascade);
|]

_createQuoteDataIndexes = [qq|
create index idx_quotecoin on quotedata (id);
create index idx_quoteunit on quotedata (unit);
create index idx_quotelastupdated on quotedata (last_updated);
create unique index idx_quotecoinprice on quotedata (id, unit, price);
create unique index idx_quotevol24h on quotedata (id, unit, volume_change_24h);
create unique index idx_quotevol1h on quotedata (id, unit, volume_change_1h);
create unique index idx_quotevol7d on quotedata (id, unit, volume_change_7dh);
create unique index idx_quotemc on quotedata (id, unit, market_cap);
create unique index idx_quotemcdom on quotedata (id, unit, market_cap_dominance);
create unique index idx_quotedilutedmc on quotedata (id, unit, diluted_market_cap);
|]

_createIndexes = ""

_createTables = _createCoinDataTable <> _createQuoteDataTable

_clearTables = [qq|
delete from coindata;
delete from quotedata;
|]

_initPrologue = [qq|
/* Database initialization Prologue */
pragma foreign_keys = ON; /*turn on foreign key constraints*/
|]

_initBody = "/* Database initialization Prologue */\n"
  <> _createTables

_initEpillogue = [qq|
/*Database initialization epilogue*/
|]

_initDB = _initPrologue <> _initBody <> _initEpillogue

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

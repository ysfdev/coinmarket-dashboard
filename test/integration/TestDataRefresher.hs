module TestDataRefresher where

import qualified Control.Concurrent as C
import qualified CoinData as CD
import qualified CoinDataStorage as CDS
import qualified Data.Time as TM

import DataRefresher
import CoinData (top10Coins)
import qualified Database.SQLite3 as DB


testStoreRefresh :: IO ()
testStoreRefresh = do 
    db <- CDS.initializeDB "./coin.db"
    storeRefreshedAt <- TM.getCurrentTime
    let t = storeRefreshedAt
    sCtx <- C.newMVar StorageContext {
        storageHandle=db, 
        lastRefreshedAt=t,
        refreshIntervalSecs=0
    }
    refreshDataStore sCtx
    topCoins <- top10Coins sCtx
    print topCoins
    DB.close db
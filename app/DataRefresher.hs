module DataRefresher
(
  MContext
, VContext
, SContext
, ViewsContext (..)
, StorageContext (..)
, Context (..)
, ViewName (..)
, RefreshInterval
, readVCtx
, writeVCtx
, refresh
, updateCurrentView
, updateSearchStr
, setErrorMessage
, clearErrorMessage
, resetSearchParams
, refreshDataStore
, elapsedTime
, incViewChangeCount
, clearViewCHangeCount
) where

import qualified Control.Concurrent as C
import qualified Control.Concurrent.MVar as M
import qualified Control.Concurrent.STM.TVar as T
import qualified Control.Concurrent.STM as STM
import qualified Control.Concurrent.Async as Async
import qualified Control.Monad as CM
import qualified Data.Vector as V
import qualified Database.SQLite3 as DB

import DataRefresherTypes
import qualified CoinData as CD
import qualified CoinDataStorage as DS
import CoinData (GetCoinsResult)
import qualified Control.Monad.Cont as CM
import qualified Data.Time as TM
import Data.Time
import MarketDataClient
import qualified MarketDataClientTypes as NT
import Data.Aeson
import Data.Aeson.Types (parse)
import Data.Aeson.Internal.Time (toPico)

readVCtx :: VContext -> IO ViewsContext
readVCtx ctx = STM.atomically $ T.readTVar ctx

writeVCtx :: VContext -> ViewsContext -> IO ()
writeVCtx v ctx = STM.atomically $ T.writeTVar v ctx

refresh :: MContext -> RefreshInterval -> IO ()
refresh ctx interval = do
  C.threadDelay interval
  c <- C.takeMVar ctx
  let v = vContext c
  vCtx <- readVCtx v
  let sCtx = sContext vCtx
  refreshDataStore sCtx
  updatedTopCoins <- CD.topNCoins sCtx $ maxDashboardCoins vCtx
  writeVCtx v vCtx { topCoins = updatedTopCoins }
  incViewChangeCount v
  M.putMVar ctx c

elapsedTime :: TM.UTCTime -> IO TM.NominalDiffTime
elapsedTime start = do 
  currentTime <- TM.getCurrentTime
  print currentTime 
  let elapsedTime = TM.diffUTCTime currentTime start
      elapsedSecs = TM.nominalDiffTimeToSeconds elapsedTime
  return elapsedTime 

-- refresh the store if we have elapsed storeDataRefreshInterval
refreshDataStore :: SContext -> IO ()
refreshDataStore ctx = do
  sCtx <- M.takeMVar ctx
  currentTime <- TM.getCurrentTime
  let lastRefreshTime = lastRefreshedAt sCtx
      refreshDeadline = toPico $ refreshIntervalSecs sCtx
      elapsedTime = TM.diffUTCTime currentTime lastRefreshTime
      elapsedSecs = TM.nominalDiffTimeToSeconds elapsedTime
  M.putMVar ctx sCtx
  -- refresh data if last update was more than 5 minutes ago
  CM.when (elapsedSecs >= refreshDeadline) $ do
    successful <- updateStore ctx
    CM.when successful $ do
      newRefreshedAt <- TM.getCurrentTime
      -- update state with lates refresh date
      sCtx <- M.takeMVar ctx
      M.putMVar ctx sCtx {lastRefreshedAt=newRefreshedAt}

updateStore :: SContext -> IO Bool
updateStore ctx = do
  let numCoins = 1000
      tdcParams = NT.QueryParams 0 numCoins
  rsp <- fetchLatestListings tdcParams
  let status = NT.status rsp
      msg = NT.message rsp
      body = NT.body rsp
      rCoins = V.mapM (parse parseJSON) body
  sc <- M.takeMVar ctx
  let db = storageHandle sc
  case rCoins of
    Error err -> do
      -- putStrLn err
      M.putMVar ctx sc
      return False
    Success coins -> do
      DS.insertCoins db coins
      numChanges <- DB.changes db
      -- putStrLn (show numChanges <> " rows changed out of " <> show numCoins <> " requested")
      M.putMVar ctx sc
      return (numChanges == numCoins)

updateCurrentView :: ViewName -> VContext -> IO ()
updateCurrentView name ctx = do
  vCtx <- readVCtx ctx
  writeVCtx ctx vCtx { currentView=name}
  incViewChangeCount ctx


updateSearchStr :: VContext -> String  -> IO ()
updateSearchStr ctx sStr = do
  vCtx <- readVCtx ctx
  writeVCtx ctx vCtx { searchStr=sStr }
  incViewChangeCount ctx

resetSearchParams :: VContext -> IO ()
resetSearchParams ctx = do
  vCtx <- readVCtx ctx
  writeVCtx ctx vCtx { searchStr=""}
  incViewChangeCount ctx

setErrorMessage :: String -> VContext -> IO ()
setErrorMessage s ctx = do
  vCtx <- readVCtx ctx
  writeVCtx ctx vCtx { errorMessage=s}
  incViewChangeCount ctx
  C.forkIO (clearErrorMessage ctx)
  return ()

clearErrorMessage :: VContext -> IO ()
clearErrorMessage ctx = do
  C.threadDelay $ round (3*10e6)
  vCtx <- readVCtx ctx
  writeVCtx ctx vCtx {errorMessage = ""}
  incViewChangeCount ctx


incViewChangeCount :: VContext -> IO ()
incViewChangeCount ctx = do
  vCtx <- readVCtx ctx
  writeVCtx ctx vCtx { changeCount = changeCount vCtx + 1 }

clearViewCHangeCount :: VContext -> IO ()
clearViewCHangeCount ctx = do
  vCtx <- readVCtx ctx
  writeVCtx ctx vCtx { changeCount = 0 }
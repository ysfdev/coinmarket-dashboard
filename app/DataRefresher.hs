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
) where

import qualified Control.Concurrent as C
import qualified Control.Concurrent.MVar as M
import qualified Control.Concurrent.STM.TVar as T
import qualified Control.Concurrent.STM as STM
import qualified Control.Concurrent.Async as Async
import qualified Control.Monad as CM

import Database.SQLite3 (Database)

import DataRefresherTypes
import qualified CoinData as CD
import qualified CoinDataStorage as CDS
import CoinData (GetCoinsResult)
import qualified Control.Monad.Cont as CM

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
  updatedTopCoins <- CD.top10Coins sCtx
  writeVCtx v vCtx { topCoins = updatedTopCoins }
  M.putMVar ctx c

updateCurrentView :: ViewName -> VContext -> IO () 
updateCurrentView name ctx = do
  vCtx <- readVCtx ctx
  writeVCtx ctx vCtx { currentView=name} 

updateSearchStr :: String -> VContext -> IO ()
updateSearchStr s ctx = do
  vCtx <- readVCtx ctx
  writeVCtx ctx vCtx { searchStr=s} 

setErrorMessage :: String -> VContext -> IO ()
setErrorMessage s ctx = do
  vCtx <- readVCtx ctx
  writeVCtx ctx vCtx { errorMessage=s} 
  C.forkIO (clearErrorMessage ctx)
  return ()

clearErrorMessage :: VContext -> IO ()
clearErrorMessage ctx = do
  C.threadDelay $ round (3*10e6)
  vCtx <- readVCtx ctx
  writeVCtx ctx vCtx {errorMessage = ""}
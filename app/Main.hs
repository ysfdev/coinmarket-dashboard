module Main where

import qualified Control.Concurrent as C
import qualified Control.Monad as Md
import qualified System.IO as SI
import qualified Control.Concurrent.MVar as M
import qualified Control.Concurrent.Async as Async

import qualified CoinData as CD
import qualified DataRefresher as DR
import Views
import qualified GHC.Profiling as C
import DataRefresher (RefreshInterval)

main :: IO ()
main = do
  SI.hSetBuffering SI.stdout SI.NoBuffering
  putStrLn "Welcome, CoinMarket Dashboard"
    -- storage <- CDS.init 
    -- topCoins <- CD.top10Coins ctx.store
  topCoins <- CD.top10Coins  -- TODO pass storeClient
  
  mCtx <- C.newMVar DR.Context {
    DR.topCoins=topCoins,
    DR.currentView=DR.Dashboard,
    DR.store="", -- TODO pass storage client
    DR.errorMessage=""
  }
  mainLoop mCtx 60


-- mainLoop refreshes the current state and UI every N seconds
mainLoop :: DR.MContext -> Int -> IO ()
mainLoop ctx ri = do
  C.forkIO $ loading ri
  Async.withAsync (DR.refresh ctx ((10 ^ 6) * ri)) $ \p -> do
    _ <- Async.wait p
    renderCurrentView ctx
    mainLoop ctx ri

-- loading displays loading N dots every given interval
loading :: DR.RefreshInterval -> IO ()
loading interval = do
  Md.replicateM_ interval (
    C.threadDelay (round (10 ^ 6)) >>
    SI.putChar '.'
    )
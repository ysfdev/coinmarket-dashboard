module Main where

import qualified Control.Concurrent as C
import qualified Control.Monad as Md
import qualified System.IO as IO
import qualified Control.Concurrent.MVar as M
import qualified Control.Concurrent.STM.TVar as T
import qualified Control.Concurrent.STM as STM
import qualified Control.Concurrent.Async as Async
import qualified System.Console.ANSI as ANSI
import Data.Char
import System.Exit (exitSuccess)
import qualified GHC.Profiling as C

import qualified CoinData as CD
import qualified CoinDataStorage as CDS
import qualified DataRefresher as DR
import qualified Views
import DataRefresher (RefreshInterval)
import GHC.IO.Handle (hFlush)
import qualified Data.Time as TM

import Database.SQLite3 (Database)

_dbLoc = "./coin.db"

main :: IO ()
main = do
  IO.hSetBuffering IO.stdout IO.NoBuffering
  IO.hSetBuffering IO.stdin IO.NoBuffering
  ANSI.clearFromCursorToScreenBeginning
  ANSI.setCursorPosition 0 0
  ANSI.setTitle "CoinMarketDashboard"
  putStrLn "Welcome, CoinMarket Dashboard"
  C.threadDelay (round (10 ^ 6)*3) -- delay for 3 seconds to welcome the user
  ANSI.clearFromCursorToScreenBeginning
  db <- CDS.initializeDB _dbLoc
  storeRefreshedAt <- TM.getCurrentTime
  sCtx <- C.newMVar DR.StorageContext { 
    DR.storageHandle=db, 
    DR.lastRefreshedAt=storeRefreshedAt,
    DR.refreshIntervalSecs=60 --seconds 
  }
  let maxDashboardCoins = 12
  topCoins <- CD.topNCoins sCtx maxDashboardCoins
  vCtx <- STM.atomically $ T.newTVar DR.ViewsContext {
    DR.topCoins=topCoins,
    DR.maxDashboardCoins=maxDashboardCoins,
    DR.currentView=DR.Dashboard,
    DR.errorMessage="",
    DR.searchStr="",
    DR.changeCount=1, -- set to 1 so we immediatley render the initial view
    DR.sContext=sCtx
  }
  mCtx <- C.newMVar DR.Context {
    DR.vContext=vCtx
  }

  let refreshIntervalSecs = 30 -- interval (in seconds) to refresh view data from DB
  C.forkIO $ viewsLoop vCtx
  C.forkIO $ mainLoop mCtx refreshIntervalSecs
  inputLoop vCtx


-- mainLoop refreshes the current dashboard data state every N seconds
mainLoop :: DR.MContext -> Int -> IO ()
mainLoop ctx ri = do
  Async.withAsync (DR.refresh ctx ((10 ^ 6) * ri)) $ \p -> do
    _ <- Async.wait p
    mainLoop ctx ri

-- viewsLoop refreshes views every 1 second
viewsLoop :: DR.VContext -> IO ()
viewsLoop ctx = do
  IO.hFlush IO.stdout
  Views.renderCurrentView ctx
  C.threadDelay (round (10 ^ 6)) -- delay for 1 second for smooth refreshing
  viewsLoop ctx

-- loading displays loading N dots every given interval
loading :: DR.RefreshInterval -> IO ()
loading interval = do
  Md.replicateM_ interval (
    C.threadDelay (round (10 ^ 6)) >>
    IO.putChar '.'
    )

-- inputLoop main method receiving user input of main app commands
inputLoop :: DR.VContext -> IO ()
inputLoop ctx = do
  putStrLn ""
  putStr "> "
  cmd <- getChar
  do
    let input = toLower cmd
    case input of
      'd' -> do 
        DR.updateCurrentView DR.Dashboard ctx
        DR.resetSearchParams ctx
      'c' -> do
        DR.updateCurrentView DR.CoinLookUp ctx
        coinLookUp ctx
      '?' -> do DR.updateCurrentView DR.Help ctx
      'q' -> exitSuccess
      _   -> do C.forkIO $ DR.setErrorMessage "Type '?' for all available cmds" ctx; return ()
    inputLoop ctx

-- coinLookUp receives user input for coin lookup searches
coinLookUp :: DR.VContext -> IO ()
coinLookUp ctx = do
  IO.hSetBuffering IO.stdin IO.LineBuffering
  input <- getLine
  if null input then coinLookUp ctx
  else do
    case head input of
      'q' -> do 
        IO.hSetBuffering IO.stdin IO.NoBuffering
        DR.updateCurrentView DR.Dashboard ctx
      _ -> do
        DR.updateSearchStr ctx input
        DR.updateCurrentView DR.CoinLookUp ctx
        coinLookUp ctx
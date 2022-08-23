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

import Database.SQLite3 (Database)

_dbLoc = "./coin.db"

main :: IO ()
main = do
  IO.hSetBuffering IO.stdout IO.NoBuffering 
  IO.hSetBuffering IO.stdin IO.NoBuffering 
  putStrLn "Welcome, CoinMarket Dashboard"
  db <- CDS.initializeDB _dbLoc
  sCtx <- C.newMVar DR.StorageContext { DR.storageHandle = db }
  topCoins <- CD.top10Coins sCtx
  vCtx <- STM.atomically $ T.newTVar DR.ViewsContext {
    DR.topCoins=topCoins,
    DR.currentView=DR.Dashboard,
    DR.errorMessage="",
    DR.searchStr="",
    DR.sContext = sCtx
  }
  mCtx <- C.newMVar DR.Context {
    DR.vContext=vCtx
  }

  let refreshIntervalSecs = 30 -- interval (in seconds) to refresh data
  C.forkIO $ viewsLoop vCtx
  C.forkIO $ mainLoop mCtx refreshIntervalSecs
  inputLoop vCtx


-- mainLoop refreshes the current state and UI every N seconds
mainLoop :: DR.MContext -> Int -> IO ()
mainLoop ctx ri = do
  Async.withAsync (DR.refresh ctx ((10 ^ 6) * ri)) $ \p -> do
    _ <- Async.wait p
    mainLoop ctx ri

viewsLoop :: DR.VContext -> IO ()
viewsLoop ctx = do
  IO.hFlush IO.stdout
  ANSI.clearFromCursorToScreenBeginning
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

inputLoop :: DR.VContext -> IO ()
inputLoop ctx = do
  putStrLn ""
  putStr "> "
  cmd <- getChar
  do
    let input = toLower cmd
    case input of
      'd' -> do DR.updateCurrentView DR.Dashboard ctx
      'c' -> do 
        DR.updateCurrentView DR.CoinLookUp ctx
        coinLookUp ctx
      '?' -> do DR.updateCurrentView DR.Help ctx
      'q' -> exitSuccess
    
      _   -> do C.forkIO $ DR.setErrorMessage "Type '?' for all available cmds" ctx; return ()
    inputLoop ctx

coinLookUp :: DR.VContext -> IO () 
coinLookUp ctx = do
  IO.hSetBuffering IO.stdin IO.LineBuffering
  IO.hSetEcho IO.stdin False
  input <- getLine
  case head input of
    'q' -> do 
      IO.hSetBuffering IO.stdin IO.NoBuffering
      DR.updateCurrentView DR.Dashboard ctx
    _ -> do 
      DR.updateSearchStr input ctx
      DR.updateCurrentView DR.CoinLookUp ctx
      coinLookUp ctx
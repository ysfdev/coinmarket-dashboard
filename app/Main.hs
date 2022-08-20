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
import qualified DataRefresher as DR
import qualified Views
import DataRefresher (RefreshInterval)

main :: IO ()
main = do
  IO.hSetBuffering IO.stdout IO.NoBuffering 
  -- TODO: clear stdin buffering
  -- IO.hSetBuffering IO.stdin IO.NoBuffering 
  putStrLn "Welcome, CoinMarket Dashboard"
    -- storage <- CDS.init 
    -- topCoins <- CD.top10Coins ctx.store
  topCoins <- CD.top10Coins  -- TODO pass storeClient
  vCtx <- STM.atomically $ T.newTVar DR.ViewsContext {
    DR.topCoins=topCoins,
    DR.currentView=DR.Dashboard,
    DR.errorMessage=""
  }
  mCtx <- C.newMVar DR.Context {
    DR.vContext=vCtx,
    DR.store="" -- TODO pass storage client
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
  ANSI.clearScreen
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
  cmd <- getLine
  if cmd == "" then do
    C.forkIO $ DR.setErrorMessage "Type '?' for all available cmds" ctx; return ()
  else do
    let input = words $ map toLower cmd
    case head input of
      "d" -> do DR.updateCurrentView DR.Dashboard ctx
      "c" -> do DR.updateCurrentView DR.CoinLookUp ctx
      "?" -> do DR.updateCurrentView DR.HelpMenu ctx
      "q" -> exitSuccess
    
      _   -> do C.forkIO $ DR.setErrorMessage "Type '?' for all available cmds" ctx; return ()
    inputLoop ctx

module DataRefresher where

import qualified Control.Concurrent as C
import qualified Control.Concurrent.MVar as M
import qualified Control.Concurrent.Async as Async
import qualified CoinData as CD
import qualified CoinDataStorage as CDS

data ViewName = Dashboard | CoinLookUp | HelpMenu deriving (Show, Eq)
type RefreshInterval = Int

data Context = Context { 
    topCoins :: CD.GetCoinsResult, -- 
    currentView :: ViewName, -- Current active view
    store :: String, -- TODO: update with actual SqlLite client type
    errorMessage :: String
} deriving (Show)

type MContext = C.MVar Context

refresh :: MContext -> RefreshInterval -> IO ()
refresh ctx interval = do
  C.threadDelay interval
  c <- C.takeMVar ctx
  updatedTopCoins <- CD.top10Coins -- TODO pass store ctx
  M.putMVar ctx c { topCoins = updatedTopCoins }


updateCurrentView :: ViewName -> MContext -> IO () 
updateCurrentView name ctx =
  C.takeMVar ctx >>= \c ->
  C.putMVar ctx c {currentView = name}

setErrorMessage :: String -> MContext -> IO ()
setErrorMessage s ctx =
  C.takeMVar ctx >>= \c ->
  C.putMVar ctx c {errorMessage = s} >>
  C.forkIO (clearErrorMessage ctx) >> return ()

clearErrorMessage :: MContext -> IO ()
clearErrorMessage ctx = do
  C.threadDelay $ round (3*10e6)
  c <- C.takeMVar ctx
  C.putMVar ctx c {errorMessage = ""}
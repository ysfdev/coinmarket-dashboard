module DataRefresher where
import qualified Control.Concurrent as C
import qualified Control.Concurrent.MVar as M
import qualified Control.Concurrent.STM.TVar as T
import qualified Control.Concurrent.STM as STM
import qualified Control.Concurrent.Async as Async

import qualified CoinData as CD
import qualified CoinDataStorage as CDS
import CoinData (GetCoinsResult)

data ViewName = Dashboard | CoinLookUp | HelpMenu deriving (Show, Eq)
type RefreshInterval = Int

type MContext = M.MVar Context
type VContext = T.TVar ViewsContext

data ViewsContext = ViewsContext {
    topCoins :: CD.GetCoinsResult, -- 
    currentView :: ViewName, -- Current active view
    errorMessage :: String
} deriving(Show)

data Context = Context { 
    vContext :: VContext,
    store :: String -- TODO: update with actual SqlLite client type
} 

refresh :: MContext -> RefreshInterval -> IO ()
refresh ctx interval = do
  C.threadDelay interval
  c <- C.takeMVar ctx
  updatedTopCoins <- CD.top10Coins -- TODO pass store ctx
  let v = vContext c
  vCtx <- readVCtx v
  writeVCtx v vCtx { topCoins = updatedTopCoins }
  M.putMVar ctx c

readVCtx :: VContext -> IO ViewsContext
readVCtx ctx = STM.atomically $ T.readTVar ctx

writeVCtx :: VContext -> ViewsContext -> IO ()
writeVCtx v ctx = STM.atomically $ T.writeTVar v ctx

updateCurrentView :: ViewName -> VContext -> IO () 
updateCurrentView name ctx = do
  vCtx <- readVCtx ctx
  writeVCtx ctx vCtx { currentView=name} 

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
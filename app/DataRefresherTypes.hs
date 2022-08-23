module DataRefresherTypes where

import qualified Control.Concurrent as C
import qualified Control.Concurrent.MVar as M
import qualified Control.Concurrent.STM.TVar as T
import qualified Control.Concurrent.STM as STM
import qualified Control.Concurrent.Async as Async

import qualified Database.SQLite3 as DB

import qualified CoinDataTypes as CD

data ViewName = Dashboard | CoinLookUp | Help deriving (Show, Eq)

type RefreshInterval = Int

type MContext = M.MVar Context
type VContext = T.TVar ViewsContext
type SContext = M.MVar StorageContext

data ViewsContext = ViewsContext {
    topCoins     :: CD.GetCoinsResult, -- latets top 10 coins
    searchStr    :: String,
    currentView  :: ViewName, -- Current active view
    errorMessage :: String,
    sContext     :: SContext
}

newtype StorageContext = StorageContext {
  storageHandle :: DB.Database
}

newtype Context = Context { 
    vContext :: VContext
} 
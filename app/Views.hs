module Views where

import qualified Control.Concurrent as C
import qualified Data.Map as Map
import           Data.Map (Map)
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           Data.Char
import           Text.Printf
import qualified System.Console.Terminal.Size as TSize
import qualified Control.Monad as M

import qualified CoinData as CD
import qualified CoinDataUtils as CDU
import qualified DataRefresher as DR
import qualified System.Posix.Internals as TSize
import Data.Aeson (Result, Object)
import Data.Aeson.Types (Value (..), Result (..), emptyObject)
import qualified Control.Monad.Cont as IO
import Data.Maybe (isNothing)
import qualified Control.Concurrent as IO
import qualified System.Console.ANSI as ANSI
import qualified Data.ByteString as T

type ScreenWidth = Int

-- renders the appropiate view based on current context selected view
renderCurrentView :: DR.VContext -> IO ()
renderCurrentView ctx = do
  size <- TSize.size
  swidth <- screenWidth size
  c <- DR.readVCtx ctx
  let currentView = DR.currentView c
      tickerName = DR.searchStr c
  mainHeader swidth currentView 
  case currentView of
    DR.Dashboard  -> printDashoard ctx swidth $ DR.topCoins c
    DR.CoinLookUp -> coinLookUp ctx
    DR.Help       -> printHelp

displayCtxErrorMsg :: DR.VContext -> IO ()
displayCtxErrorMsg ctx = do
  c <- DR.readVCtx ctx
  let msg = DR.errorMessage c
  M.when (msg /= "") $ putStrLn msg

-- ##################################################################################################
-- ######################################## Help View ###############################################
-- ##################################################################################################

printHelp :: IO ()
printHelp = do
  putStrLn "\n\
   \Available Commands: \n\
   \      D             Invoke the main dashboard     ; displays the top 10 coins \n\
   \      C <tickerID>  Invoke the detailed coin view ; displays details about a specific coin \n\
   \      ?             Invoke the help menu          ; displays all available commands \n\
   \      Q             Quit the application"

verticalHelp :: IO ()
verticalHelp = putStrLn "Available Commands: D (Dashboard), C (Coin Search), Q (Quit), ? (Help)"

-- ##################################################################################################
-- ######################################## Dashboard View ##########################################
-- ##################################################################################################

-- Print a list of Coins to stdout as a table
printDashoard :: DR.VContext -> ScreenWidth -> CD.GetCoinsResult -> IO ()
printDashoard ctx sw topCoins = do
  putStrLn ""
  let slotsLength = calcSlotsLength sw $ length dashboardRowsHeader
  case topCoins of
    CD.GcrNotFoundError    -> do 
      putStrLn "Unexpected Database Error" -- TODO: setErrorMessage in context
      footer
    CD.GcrUnexpectedError  -> do
      putStrLn "Unexpected Server Error" -- TODO: setErroMessage in context
      footer
    CD.GcrCoinList coins   -> do
      putStrLn . showRow . addPadding slotsLength $ rowNameLength dashboardRowsHeader -- header row
      putStrLn $ rowSeparator slotsLength -- line breaker
      -- display coins data
      (putStrLn . 
        unlines . -- combine all lines to single sting
        interleave (rowSeparator slotsLength)) 
        (map (showRow . ((addPadding slotsLength . rowNameLength) . showCoin)) 
        (V.toList coins))
      footer

-- Show a Coin as a list of Strings
showCoin :: CD.Coin -> [String]
showCoin c = [
    strResult $ CDU._coinName c,
    strResult $ CDU._coinSymbol c,
    fmtNum  $ fracResult $ CDU._coinQPrice c "USD",
    fmtNum  $ fracResult $ CDU._coinQPercentChange24h c "USD",
    show (intResult $ CDU._coinCmcRank c)
  ]
  where usdInfo = mapResult (CDU._coinQuoteMap c) Map.! "USD"


-- Pair strings in a list with thier corresponding lengths
rowNameLength :: [String] -> [(String, Int)]
rowNameLength xs = zip xs (map length xs)

-- Each slot in a row should have a total length of slotLength
-- Add padding for each string in a slot on the left/right depending on the length of the string
addPadding :: ScreenWidth -> [(String, Int)] -> [String]
addPadding sw xs = [leftPadding y ++ x ++ rightPadding y | (x, y) <- xs]
                where
                  leftPadding n  = concat $ replicate (padding sw n) " "
                  rightPadding n
                    | even n     = concat $ replicate (padding sw n) " "
                    | otherwise  = concat $ replicate (padding sw n + 1) " "

-- Transform a list of strings into a single strng representing a row in the dashboard
showRow :: [String] -> String
showRow xs = concat $ interleave "|" xs

-- ##################################################################################################
-- ######################################## Coin View ###############################################
-- ##################################################################################################

coinLookUp :: DR.VContext -> IO ()
coinLookUp vCtx = do
  viewData <- DR.readVCtx vCtx
  let 
    sStr   = DR.searchStr viewData
    sCtx   = DR.sContext viewData
  if null sStr then do coinLookupMenu
  else do
    case buildSearchParams sStr of 
      (Just p) -> do
        let searchValue = getSearchValue sStr
        putStrLn $ searchTypeToStr p ++ " search result that " ++ searchStyleToStr p ++ " value: " ++ searchValue ++ "\n"
        coin <- CD.coinLookup sCtx p
        case coin of
          CD.ClrNotFoundError    -> do
            putStrLn $ "Coin with ticker " ++ sStr ++ " not present in the database"
            coinLookupMenu
          CD.ClrUnexpectedError  -> do
            putStrLn "Unexpected Server Error. Please try again"
            coinLookupMenu
          CD.ClrCoin coin        -> do
            putStrLn $ showCoinInfo coin
            coinLookupMenu
      _ -> do
        putStrLn $ "\n Invalid search terms" ++ sStr ++ ".Please try again... \n"
        IO.threadDelay $ (10 * 6) * 3
        DR.resetSearchParams vCtx
        coinLookupMenu

buildSearchParams :: String -> Maybe CD.CoinLookupParams
buildSearchParams str
  -- We should only expect 2 values for search ["{SearchType}.{SearchStyle}", "{SearchValue}"]
  | length values >= 2 =
    let srcValue   = head $ tail values
        srcFilters = filter (/='.') $ head values
        srcTerm    = buildSearchTerm (head srcFilters) srcValue 
        srcStyle   = buildSearchStyle $ head $ tail srcFilters
    in Just $ CD.CoinLookupParams srcTerm srcStyle
  | otherwise = Nothing
  where values = filter (/="=") $ words str

getSearchValue :: String -> String 
getSearchValue = head . tail . filter (/="=") . words

searchTypeToStr :: CD.CoinLookupParams -> String
searchTypeToStr p = 
  case CD.clpSearchData p of 
    (CD.ClsdName _) -> "Name"
    (CD.ClsdSlug _) -> "Slug"
    (CD.ClsdSymbol _) -> "Symbol"


searchStyleToStr :: CD.CoinLookupParams -> String
searchStyleToStr p = 
  case CD.clpSearchStyle p of 
    CD.CtssContain -> "containing"
    CD.CtssEnd     -> "ending with"
    CD.CtssBegin   -> "beginning with"

buildSearchTerm :: Char -> String -> CD.CoinLookupSearchData
buildSearchTerm sType sStr =
    case toUpper sType of 
    'N' -> CD.ClsdName sStr
    'L' -> CD.ClsdSlug sStr
    _   -> CD.ClsdSymbol sStr
  

buildSearchStyle :: Char -> CD.CoinTextSearchStyle
buildSearchStyle sStyle =
  case toUpper sStyle of 
    'C' -> CD.CtssContain
    'E' -> CD.CtssEnd
    _   -> CD.CtssBegin

coinLookupMenu :: IO ()
coinLookupMenu = do
  putStrLn "\n\
    \ ++++++++++++++++++++++++++++++++++++++++++++++++++++++ \n\
    \             ADVANCED COIN SEARCH                       \n\ 
    \ ------------------------------------------------------ \n\
    \ Search Type: S(Symbol), N(Name) L(Slug)                \n\
    \ Search Style: B(Begins With), C(Contains), E(Ends With)\n\
    \ Usage: (SearchType.SearchTyle SearchTerm)              \n\
       \ > (s.b btc), Searches a Symbol Beggining with 'btc' \n\
       \ > (n.c ada), Searches a Name that contains 'ada'    \n\
    \ ++++++++++++++++++++++++++++++++++++++++++++++++++++++ "
  putStr "Enter Search Term or q to go back: "

showCoinInfo :: CD.Coin -> String
showCoinInfo c =
  "|  Name:         " ++ strResult (CDU._coinName c)                             ++ "\n" ++
  "|  Symbol:       " ++ strResult (CDU._coinSymbol c)                           ++ "\n" ++
  "|  Rank:         " ++ show (intResult $ CDU._coinCmcRank c)                   ++ "\n" ++
  "|  Price:        " ++ show (fmtNum $ fracResult $ CDU._coinQPrice c "USD")    ++ "\n" ++
  "|  24Hr %:       " ++ show (fmtNum $ fracResult $ CDU._coinQPercentChange24h c "USD")  ++ "\n" ++
  "|  Total Supply: " ++ show (round $ fracResult $ CDU._coinTotalSupply c)                           ++ "\n" ++
  "|  Market Cap:   " ++ show (round $ fracResult $ CDU._coinQMarketCap c "USD")
  where usdInfo = mapResult (CDU._coinQuoteMap c) Map.! "USD"

-- ##################################################################################################
-- ######################################## Helper Functions ########################################
-- ##################################################################################################

-- display main header on the center of screen
mainHeader :: Int -> DR.ViewName -> IO ()
mainHeader swidth view = do
  let header = "COIN MARKET CLI APP"
  putStrLn $ rowSeparatorV2 swidth
  putStrLn $ replicate (swidth `div` 2 - length header) ' ' ++ header
  putStrLn $ replicate (swidth `div` 2 - length (show view) - 5) ' ' ++ map toUpper (show view)
  putStrLn $ rowSeparatorV2 swidth

dashboardRowsHeader :: [String]
dashboardRowsHeader = ["Name", "Symbol", "Price", "24H%", "Rank"]

footer :: IO ()
footer = putStrLn "Source: CoinMarketCap" >> verticalHelp

rowSeparator :: Int -> String
rowSeparator length = replicate (length * 5 + 3) '-'

rowSeparatorV2 :: ScreenWidth -> String
rowSeparatorV2 sw = replicate sw '-'

padding :: Int -> Int -> Int
padding length n = div (length - n) 2

interleave :: a -> [a] -> [a]
interleave x []     = []
interleave x [y]    = [y]
interleave x (y:ys) = y : x : interleave x ys

-- truncate decimal places
roundToStr :: (PrintfArg a, Floating a) => Int -> a -> String
roundToStr = printf "%0.*f"

fmtNum :: (PrintfArg a, Floating a) => a -> String
fmtNum = roundToStr 2

-- returns the screen width from a Window size
screenWidth :: Maybe (TSize.Window ScreenWidth) -> IO ScreenWidth
screenWidth Nothing = return 0
screenWidth (Just s) = return $ TSize.width s

-- calculate the slots length based on screen width and total desired slots 
calcSlotsLength :: ScreenWidth -> Int -> Int
calcSlotsLength sw totalSlots
  | even sl = sl
  | otherwise = sl - 1 -- reduce 1 width pixel to feet all values properly 
 where sl = sw `div` totalSlots

strResult :: Show a => Result a -> String
strResult r = case r of Success a -> show a; _  -> ""

intResult :: Integral a => Result a -> Int
intResult r = case r of Success a -> fromIntegral a; _  -> 0

fracResult :: Fractional a => Result a -> a
fracResult r = case r of Success a -> a; _  -> 0

mapResult :: Result (Map String Object) -> Map String Object
mapResult r = case r of Success a -> a; _  -> Map.empty

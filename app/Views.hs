module Views where

import qualified Control.Concurrent as C
import qualified Data.Map as Map
import           Data.Map (Map)
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           Data.Char
import           Text.Printf
import qualified System.Console.Terminal.Size as TSize

import           CoinData
import qualified DataRefresher as DR
import qualified System.Posix.Internals as TSize

type ScreenWidth = Int

-- renders the appropiate view based on current context selected view
renderCurrentView :: DR.VContext -> IO ()
renderCurrentView ctx = do
  size <- TSize.size
  swidth <- screenWidth size
  print "screen Size"
  print swidth
  c <- DR.readVCtx ctx
  let currentView = DR.currentView c
  mainHeader swidth currentView
  case currentView of
    DR.Dashboard  -> printDashoard swidth $ DR.topCoins c
    DR.CoinLookUp -> putStrLn "Coin View" -- TODO: display coinLookUp
    DR.Help       -> printHelp


-- ##################################################################################################
-- ######################################## Help View ###############################################
-- ##################################################################################################

printHelp :: IO ()
printHelp = do
  putStrLn ""
  putStrLn "Available Commands: "
  putStrLn ""
  putStrLn "      D             Invoke the main dashboard     ; displays the top 10 coins"
  putStrLn "      C <tickerID>  Invoke the detailed coin view ; displays details about a specific coin"
  putStrLn "      ?             Invoke the help menu          ; displays all available commands"
  putStrLn "      Q             Quit the application"
  putStrLn ""

verticalHelp :: IO ()
verticalHelp = putStrLn "Available Commands: D (Dashboard), C (Ticker), Q (Quit), ? (Help)"

-- ##################################################################################################
-- ######################################## Dashboard View ##########################################
-- ##################################################################################################

-- Print a list of Coins to stdout as a table
printDashoard :: ScreenWidth -> GetCoinsResult -> IO ()
printDashoard sw topCoins = do
  putStrLn ""
  let slotsLength = calcSlotsLength sw $ length dashboardRowsHeader
  case topCoins of
    GcrNotFoundError    -> putStrLn "Unexpected Database Error" -- TODO: setErrorMessage in context
    GcrUnexpectedError  -> putStrLn "Unexpected Server Error" -- TODO: setErroMessage in context
    GcrCoinList coins   -> do
      putStrLn . showRow . addPadding slotsLength $ rowNameLength dashboardRowsHeader
      putStrLn $ rowSeparator slotsLength
      (putStrLn . unlines . interleave (rowSeparator slotsLength)) (map (showRow . ((addPadding slotsLength . rowNameLength) . showCoin)) (V.toList coins))
      footer

-- Show a Coin as a list of Strings
showCoin :: Coin -> [String]
showCoin c = [_coinName c, _coinSymbol c, roundToStr 2 (_coinQPrice usdInfo), roundToStr 2 (_coinQPercentChange24h usdInfo), show $ _coinCmcRank c]
              where usdInfo = _coinQuoteMap c Map.! "USD"

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

-- pretty print a coin 
showCoinInfo :: Maybe Coin -> String
showCoinInfo m =
  case m of
    Nothing -> "Coin not present in the database"
    Just c  -> "  Name:         " ++ _coinName c                             ++ "\n" ++
               "  Symbol:       " ++ _coinSymbol c                           ++ "\n" ++
               "  Rank:         " ++ show (_coinCmcRank c)                   ++ "\n" ++
               "  Price:        " ++ show (_coinQPrice usdInfo)              ++ "\n" ++
               "  24Hr %:       " ++ show (_coinQPercentChange24h usdInfo)   ++ "\n" ++
               "  Total Supply: " ++ show (_coinTotalSupply c)               ++ "\n" ++
               "  Market Cap:   " ++ show (_coinQMarketCap usdInfo)
                where usdInfo = _coinQuoteMap c Map.! "USD"

showCoinInfoNew :: Coin -> String
showCoinInfoNew c =
               "  Name:         " ++ _coinName c                             ++ "\n" ++
               "  Symbol:       " ++ _coinSymbol c                           ++ "\n" ++
               "  Rank:         " ++ show (_coinCmcRank c)                   ++ "\n" ++
               "  Price:        " ++ show (_coinQPrice usdInfo)              ++ "\n" ++
               "  24Hr %:       " ++ show (_coinQPercentChange24h usdInfo)   ++ "\n" ++
               "  Total Supply: " ++ show (_coinTotalSupply c)               ++ "\n" ++
               "  Market Cap:   " ++ show (_coinQMarketCap usdInfo)
                where usdInfo = _coinQuoteMap c Map.! "USD"

-- Get a coin by ticker ID if it exists in the database
--getCoin :: String -> Maybe Coin
--getCoin ticker
--  | null coin         = Nothing 
--  | otherwise         = Just $ head coin
--    where coin = filter (\s -> _coinSymbol s == (map toUpper ticker)) coins

-- Print coin info to stdout if it exists in the database
--printCoin :: String -> IO ()
--printCoin ticker =
--  if null ticker || not (length ticker == 3) then do 
--    putStrLn ""
--    putStrLn "Please enter the ticker ID you want to query"
--    putStrLn "Usage: C <tickerID>"
--  else do 
--    putStrLn ""
--    putStrLn . showCoinInfo $ getCoin ticker
--    putStrLn ""
--    putStrLn footer

-- getCoinNew :: String -> IO CoinLookupResult
-- getCoinNew ticker = do 
--   let params = CoinLookupParams (map toUpper ticker) 
--   coin <- coinLookUp params 

-- printCoinNew :: String -> IO ()
-- printCoinNew ticker =
--   if null ticker || not (length ticker == 3) then do 
--     putStrLn ""
--     putStrLn "Please enter the ticker ID you want to query"
--     putStrLn "Usage: C <tickerID>"
--   else do 
--     coin <- getCoinNew ticker
--     putStrLn ""
--     case coin of 
--       ClrNotFoundError    -> putStrLn $ "Coin with ticker " ++ ticker ++ " not present in the database"
--       ClrUnexpectedError  -> putStrLn "Unexpected Server Error"
--       ClrCoin             -> do
--         putStrLn . showCoinInfoNew coin
--         putStrLn ""
--         footer

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

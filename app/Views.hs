module Views where 

import qualified Control.Concurrent as C
import qualified Data.Map as Map
import           Data.Map (Map)
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           Data.Char
import           Text.Printf
import           CoinData
import qualified DataRefresher as DR

-- ##################################################################################################
-- ######################################## Help View ###############################################
-- ##################################################################################################

renderCurrentView :: DR.VContext -> IO ()
renderCurrentView ctx = do 
  -- TODO: @Andres implement logic to render view based on the ctx.currentView
  -- Use System.Console.ANSI to clear screen after each view rendering
  c <- DR.readVCtx ctx
  case DR.currentView c of 
    DR.Dashboard     -> do 
      printDashoardNew $ DR.topCoins c
      printHelp
    DR.CoinLookUp    -> putStrLn "Coin View"
    DR.HelpMenu      -> printHelp


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

-- ##################################################################################################
-- ######################################## Dashboard View ##########################################
-- ##################################################################################################

slotLength :: Int
slotLength = 20

type Dashboard = [Coin]

-- Show a Coin as a list of Strings
showCoin :: Coin -> [String]
showCoin c = [_coinName c, _coinSymbol c, roundToStr 2 (_coinQPrice $ usdInfo), roundToStr 2 (_coinQPercentChange24h $ usdInfo), show $ _coinCmcRank c]
              where usdInfo = _coinQuoteMap c Map.! "USD"

-- Pair strings in a list with thier corresponding lengths
rowNameLength :: [String] -> [(String, Int)]
rowNameLength xs = zip xs (map length xs)

-- Each slot in a row should have a total length of slotLength
-- Add padding for each string in a slot on the left/right depending on the length of the string
addPadding :: [(String, Int)] -> [String]
addPadding xs = [(leftPadding y) ++ x ++ (rightPadding y) | (x, y) <- xs]
                where 
                  leftPadding n  = concat $ replicate (padding n) " "
                  rightPadding n 
                    | mod n 2 == 0    = concat $ replicate (padding n) " "
                    | otherwise       = concat $ replicate ((padding n) + 1) " "

-- Transform a list of strings into a single strng representing a row in the dashboard
showRow :: [String] -> String
showRow xs = concat $ interleave "|" xs

-- Print a list of Coins to stdout as a table
printDashoard :: Dashboard -> IO ()
printDashoard d = do
  putStrLn ""
  putStrLn . showRow . addPadding $ rowNameLength header
  putStrLn rowSeparator
  putStrLn . unlines . interleave rowSeparator . map showRow . map addPadding . map rowNameLength $ map showCoin d
  putStrLn footer


-- for integration with top10Coins from DB module

printDashoardNew :: GetCoinsResult -> IO ()
printDashoardNew topCoins = do
  putStrLn ""
  case topCoins of 
    GcrNotFoundError    -> putStrLn "Unexpected Database Error"
    GcrUnexpectedError  -> putStrLn "Unexpected Server Error"
    GcrCoinList coins   -> do
      putStrLn . showRow . addPadding $ rowNameLength header
      putStrLn rowSeparator
      putStrLn . unlines . interleave rowSeparator . map showRow . map addPadding . map rowNameLength $ map showCoin (V.toList coins)
      putStrLn footer

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
               "  Price:        " ++ show (_coinQPrice $ usdInfo)            ++ "\n" ++
               "  24Hr %:       " ++ show (_coinQPercentChange24h $ usdInfo) ++ "\n" ++ 
               "  Total Supply: " ++ show (_coinTotalSupply c)               ++ "\n" ++ 
               "  Market Cap:   " ++ show (_coinQMarketCap $ usdInfo)  
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

{-

-- for integration with coinLookUp from DB module

showCoinInfoNew :: Coin -> String
showCoinInfoNew m = 
               "  Name:         " ++ _coinName c                             ++ "\n" ++ 
               "  Symbol:       " ++ _coinSymbol c                           ++ "\n" ++
               "  Rank:         " ++ show (_coinCmcRank c)                   ++ "\n" ++   
               "  Price:        " ++ show (_coinQPrice $ usdInfo)            ++ "\n" ++
               "  24Hr %:       " ++ show (_coinQPercentChange24h $ usdInfo) ++ "\n" ++ 
               "  Total Supply: " ++ show (_coinTotalSupply c)               ++ "\n" ++ 
               "  Market Cap:   " ++ show (_coinQMarketCap $ usdInfo)  
                where usdInfo = _coinQuoteMap c Map.! "USD"

getCoinNew :: String -> IO CoinLookupResult
getCoinNew ticker = do 
  let params = CoinLookupParams (map toUpper ticker) ... 
  coin <- coinLookUp params 

printCoinNew :: String -> IO ()
printCoinNew ticker =
  if null ticker || not (length ticker == 3) then do 
    putStrLn ""
    putStrLn "Please enter the ticker ID you want to query"
    putStrLn "Usage: C <tickerID>"
  else do 
    coin <- getCoinNew ticker
    putStrLn ""
    case coin of 
      ClrNotFoundError    -> putStrLn $ "Coin with ticker " ++ ticker ++ " not present in the database"
      ClrUnexpectedError  -> putStrLn "Unexpected Server Error"
      ClrCoin             -> do
        putStrLn . showCoinInfoNew coin
        putStrLn ""
        putStrLn footer
-}

-- ##################################################################################################
-- ######################################## Helper Functions ########################################
-- ##################################################################################################

-- Header row for dashboard view
header :: [String]
header = ["Name", "Symbol", "Price", "24H%", "Rank"]

rowSeparator :: String
rowSeparator = replicate (slotLength * 5 + 5) '-'

footer :: String 
footer = "Source: CoinMarketCap"

padding :: Int -> Int
padding n = div (slotLength - n) 2

interleave :: a -> [a] -> [a] 
interleave x []     = []
interleave x [y]    = [y]
interleave x (y:ys) = y : x : interleave x ys

-- clear the screen
clear :: IO ()
clear = putStr "\ESC[2J"

-- truncate decimal places
roundToStr :: (PrintfArg a, Floating a) => Int -> a -> String
roundToStr = printf "%0.*f"

-- ##################################################################################################
-- ######################################## Sample Data #############################################
-- ##################################################################################################

{-

data CoinQuote = CoinQuote
  {
    _coinQPrice :: Double,
    _coinQPercentChange24h :: Double,
    _coinQMarketCap :: Int
  } deriving Show

data Coin = Coin 
  {
    _coinName    :: String, 
    _coinSymbol  :: String,
    _coinCmcRank    :: Int,
    _coinTotalSupply :: Int,
    _coinQuoteMap :: Map String CoinQuote
  }
  deriving Show

btcUsdtQuote :: CoinQuote 
btcUsdtQuote = CoinQuote 24500.05 7.3 456853370715

adaUsdtQuote :: CoinQuote 
adaUsdtQuote = CoinQuote 0.5 2.6 18049512381

ethUsdtQuote :: CoinQuote 
ethUsdtQuote = CoinQuote 18200.42 12.1 223036692722

solUsdtQuote :: CoinQuote 
solUsdtQuote = CoinQuote 42.1 5.9 14654722328

btcMap :: Map String CoinQuote
btcMap = Map.fromList [("USD", btcUsdtQuote)]

adaMap :: Map String CoinQuote
adaMap = Map.fromList [("USD", adaUsdtQuote)]

ethMap :: Map String CoinQuote
ethMap = Map.fromList [("USD", ethUsdtQuote)]

solMap :: Map String CoinQuote
solMap = Map.fromList [("USD", solUsdtQuote)]

coin1 :: Coin 
coin1 = Coin "Bitcoin" "BTC" 1 19117693 btcMap

coin2 :: Coin 
coin2 = Coin "Cardano" "ADA" 7 42752565071 adaMap

coin3 :: Coin 
coin3 = Coin "Etherium" "ETH" 2 121906640 ethMap

coin4 :: Coin 
coin4 = Coin "Solana" "SOL" 9 348408611 solMap

coins :: Dashboard 
coins = [coin1, coin2, coin3, coin4]


-}

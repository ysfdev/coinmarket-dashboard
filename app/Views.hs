module Views where 

-- ##################################################################################################
-- ######################################## Help View ########################################
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

-- ##################################################################################################
-- ######################################## Dashboard View ########################################
-- ##################################################################################################

slotLength :: Int
slotLength = 20

type Dashboard = [Coin]

-- Show a Coin as a list of Strings
showCoin :: Coin -> [String]
showCoin c = [name c, symbol c, show $ price c, show $ hr24Chg c, show $ rank c]

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

-- ##################################################################################################
-- ######################################## Sample Data ########################################
-- ##################################################################################################

data Coin = Coin 
  {
    name    :: String, 
    symbol  :: String,
    price   :: Float,
    hr24Chg :: Float,
    rank    :: Int
  }
  deriving Show

coin1 :: Coin 
coin1 = Coin "Bitcoin" "BTC" 23000.2 5.2 1

coin2 :: Coin 
coin2 = Coin "Cardano" "ADA" 0.5 2.3 5

coin3 :: Coin 
coin3 = Coin "Etherium" "ETH" 1525.3 10.1 2

coin4 :: Coin 
coin4 = Coin "Solana" "SOL" 38.5 4.6 9

coins :: Dashboard 
coins = [coin1, coin2, coin3, coin4]

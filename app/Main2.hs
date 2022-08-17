module Main where 

import System.IO
import Data.Char
import Views 

main :: IO ()
main = do 
  printHelp 
  subsequentInvokes

subsequentInvokes :: IO ()
subsequentInvokes = do
  putStrLn ""
  putStr "> "
  hFlush stdout
  cmd <- getLine
  if cmd == "" then do
    putStrLn "\nType '?' for all available cmds"; subsequentInvokes
  else do
    let input = words $ map toLower cmd
    case head input of
      "d" -> do printDashoard coins; subsequentInvokes
      "c" -> do printCoin . last $ input; subsequentInvokes
      "?" -> do printHelp; subsequentInvokes
      "q" -> return ()
      _   -> do putStrLn "\nType '?' for all available cmds"; subsequentInvokes

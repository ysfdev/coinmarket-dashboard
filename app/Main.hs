module Main where 

import System.IO
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
    printHelp; subsequentInvokes
  else do
    let input = words cmd
    case head input of
      "D" -> do printDashoard coins; subsequentInvokes
      "C" -> do printCoin . last $ input; subsequentInvokes
      "?" -> do printHelp; subsequentInvokes
      "Q" -> return ()
      _   -> do putStrLn "\nType '?' for all available cmds"; subsequentInvokes

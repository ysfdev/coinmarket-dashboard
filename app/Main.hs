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
  case cmd of 
    "D" -> do printDashoard coins; subsequentInvokes
    "C" -> do printDashoard coins; subsequentInvokes
    "?" -> do printHelp; subsequentInvokes
    "Q" -> return ()
    _   -> do putStrLn "Type '?' for all available cmds"; subsequentInvokes

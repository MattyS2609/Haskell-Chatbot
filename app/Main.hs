module Main where

import LGPT.Helpers
import LGPT.TUI qualified as TUI
import Configuration.Dotenv (loadFile, defaultConfig, onMissingFile)


{- | This is what gets run when you run the program. 

    It just calls the runREPL function in TUI.hs, which is where the real work 
    happens :)
-}
main :: IO ()
main = do
  -- Pre-initialisation to set up the terminal
  runStart
  onMissingFile (loadFile defaultConfig) (putStrLn "The .env file could not be found so no football queries can be used.") -- loads the .env file using dotenv, if the file cannot be found an appropriate error message is displayed 


  -- Actually run the loop!
  TUI.runREPL   
module Main where

import Control.Monad
import System.Environment
import System.IO (hPutStrLn, stdout, stderr)
import System.Exit
import qualified SML (parseStr, parseFile)
import Mermaid.FSM as Mermaid (write)

main :: IO ()
main = do
  args <- getArgs
  table <- case args of
            [file] -> SML.parseFile file
            [] -> liftM SML.parseStr $ getContents
  case table of
    Right transitions -> do
               Mermaid.write stdout transitions
               exitWith ExitSuccess
    Left error -> do
               hPutStrLn stderr $ show error
               exitWith(ExitFailure 1)

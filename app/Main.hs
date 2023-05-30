module Main where

import Control.Monad
import System.Console.GetOpt
import System.Environment
import System.IO (hPutStrLn, stdout, stderr)
import System.Exit
import qualified SML (parseStr, parseFile)
import Mermaid.FSM as Mermaid (write)
import PlantUML.FSM as PlantUML (write)

data ChartType = PlantUML | Mermaid
               deriving (Eq, Show, Read)
data Options = Options { chartType :: ChartType
                       }

defaultOptions = Options { chartType = PlantUML
                         }

options :: [OptDescr (Options -> IO Options)]
options = [
 Option "t" ["type"]
        -- TODO: validate
        (ReqArg (\arg opt -> return opt { chartType = read arg })
                "PlantUML | Mermaid")
        "Diagram type",
 Option "h" ["help"]
        (NoArg (\_ -> do
                  prg <- getProgName
                  putStrLn $ usageInfo prg options
                  exitWith ExitSuccess))
         "Show help"
 ]

main :: IO ()
main = do
  args <- getArgs
  let (actions, nonOptions, errors) = getOpt RequireOrder options args
  opts <- foldl (>>=) (return defaultOptions) actions

  table <- case nonOptions of
            [file] -> SML.parseFile file
            [] -> liftM SML.parseStr $ getContents
  case table of
    Right transitions -> do
               case chartType opts of
                 Mermaid -> Mermaid.write stdout transitions
                 PlantUML -> PlantUML.write stdout transitions
               exitWith ExitSuccess
    Left error -> do
               hPutStrLn stderr $ show error
               exitWith(ExitFailure 1)

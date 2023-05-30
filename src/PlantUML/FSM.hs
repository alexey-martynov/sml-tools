module PlantUML.FSM (write, writeFsm) where

import Data.List (intercalate)
import Data.Text (pack, replace, unpack)
import System.IO (Handle, hPutStr, hPutStrLn)
import FSM(Guard, State(..), Action, Event, Row(..), Table)

formatRow :: Row -> String
formatRow (Row source event guards action dest) = format source
                                                  ++ " --> "
                                                  ++ format dest
                                                  ++ formatTransition event guards action
    where
      format Initial = "[*]"
      format Final = "[*]"
      format (State name) = unpack $ replace (pack "::") (pack ".") $ pack name

      formatEvent Nothing = ""
      formatEvent (Just event) = (unpack $ replace (pack "::") (pack ".") $ pack event) ++ " "

      formatGuards [] = ""
      formatGuards guards = "[ " ++ intercalate "∧" guards ++ " ] "

      formatAction Nothing = ""
      formatAction (Just action) = "/ " ++ (unpack $ replace (pack "::") (pack ".") $ pack action)

      formatTransition event guards action = case description of
                                               [] -> ""
                                               s -> ": " ++ s
          where description = formatEvent event ++ formatGuards guards ++ formatAction action

writeFsm :: Table -> String
writeFsm table = concat $ map (putStrLn . formatRow) table
    where
      putStrLn str = "    " ++ str ++ "\n"

write :: Handle -> Table -> IO ()
write file table = do
    hPutStrLn file "@startuml"
    hPutStr file $ writeFsm table
    hPutStrLn file "@enduml"

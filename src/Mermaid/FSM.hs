module Mermaid.FSM (write) where

import System.IO (Handle, hPutStr, hPutStrLn)
import FSM(Table)
import PlantUML.FSM (writeFsm)

write :: Handle -> Table -> IO ()
write file table = do
    hPutStrLn file "stateDiagram-v2"
    hPutStr file $ writeFsm table

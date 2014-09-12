
module ConsoleLog (consoleLog) where

import Haste.Foreign
import Haste.Prim
import Haste.App
import JSEscape

consoleLog :: String -> Client ()
consoleLog str = 
    liftIO $ ffi $ toJSStr ("console.log('" ++ jsEscape str ++ "');")

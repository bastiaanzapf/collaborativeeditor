
module ConsoleLog (consoleLog) where

import Haste.Foreign
import Haste.Prim
import Haste.App

jsEscape ('\'':tc) = '\\':'\'':jsEscape tc
jsEscape (x:tc) = x:jsEscape tc
jsEscape [] = []

consoleLog :: String -> Client ()
consoleLog str = 
    liftIO $ ffi $ toJSStr ("console.log('" ++ jsEscape str ++ "');")

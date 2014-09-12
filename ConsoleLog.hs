
module ConsoleLog (consoleLog) where

import Haste.Foreign
import Haste.Prim

jsEscape ('\'':tc) = '\\':'\'':jsEscape tc
jsEscape (x:tc) = x:jsEscape tc
jsEscape [] = []

consoleLog :: String -> IO ()
consoleLog str = 
    ffi $ toJSStr ("console.log('" ++ jsEscape str ++ "');")


module Profile (profile) where

import Haste
import Haste.App
import Haste.Prim
import Haste.Foreign

showCurrentDate :: Client ()
showCurrentDate = liftIO $ ffi $ toJSStr "d=new Date();console.log(d.getMilliseconds());"

profile :: String -> Client a -> Client a
profile key op = do showCurrentDate
                    x <- op
                    showCurrentDate
                    return x
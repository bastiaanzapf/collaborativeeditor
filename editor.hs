
import Haste
import Haste.Prim
import Haste.App
--import Haste.App.Concurrent
import Haste.Foreign

import Haste.WebSockets

import JSHash

newtype Id = Mk_Id (Int,Int)

data W_Character = Mk_W_Character { id          :: Id
                                  , visible     :: Bool
                                  , literal     :: Char
                                  , previous_id :: Id
                                  , next_id     :: Id    }


consoleLog :: String -> IO ()
consoleLog str = ffi $ toJSStr ("console.log('" ++ str ++ "');")

clientMain :: IO ()
clientMain = withElems ["editor"] $ 
    \[editor] ->
    do setProp editor "innerHTML" "0123456789"
       th <- newHash "test"::IO (JSHash String (Int,Int))
       storeHash th "test" (2,5)
       a<-readHash th "test"
       x<- (getProp editor "innerHTML")
       setProp editor "innerHTML" $ show a
       return ()


append ws str = withElems ["editor"] $
   \[editor] ->
   do  x<- getProp editor "innerHTML"
       setProp editor "innerHTML" (x ++ (fromJSStr str))

send ws = do wsSend ws $ toJSStr "roundtriptest"

-- | Launch the application!
main :: IO ()
main = concurrent $ do x<-newEmptyMVar
                       liftIO $ clientMain
--                       withWebSocket "ws://localhost:9000/ws" append (return ()) send
                       return ()



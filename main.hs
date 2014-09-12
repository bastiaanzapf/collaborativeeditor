
import Haste
import Haste.Prim
import Haste.App
import Haste.App.Concurrent
import Haste.Foreign

import Haste.WebSockets

import JSHash
import Editor
import ConsoleLog

import Data.IORef
import Control.Monad
import Control.Applicative


data API = API {
    apiHello :: Remote (Server Int)
  }


--newtype Id = Mk_Id (Int,Int) deriving (Eq, Show, Read)

id_Begin = Mk_Id (0,0)
id_End   = Mk_Id (9999999,0)


wc_begin = W_Character {Editor.id=id_Begin,visible=False,literal=' ',previous_id=id_Begin,next_id=id_End}
wc_end = W_Character {Editor.id=id_End,visible=False,literal=' ',previous_id=id_End,next_id=id_Begin}

wc1 = W_Character {Editor.id=Mk_Id (1,1),visible=True,literal='a',previous_id=id_Begin,next_id=id_End}

wc2 = W_Character {Editor.id=Mk_Id (1,2),visible=True,literal='b',previous_id=id_Begin,next_id=id_End}

wc3 = W_Character {Editor.id=Mk_Id (1,4),visible=True,literal='c',previous_id=id_Begin,next_id=id_End}

clientMain :: API -> Client ()
clientMain api = withElems ["editor"] $ \[editor] -> do 
       setProp editor "innerHTML" "0123456789"
       content <- newHash "content" :: Client (JSHash Id W_Character)
       op_pool <- newHash "pool" :: Client (JSHash Int String)

       let storeInContent x = storeHash content (Editor.id x) x

       storeInContent wc_begin
       storeInContent wc_end

       seq <- subseq content id_Begin id_End
--       consoleLog $ show $ tail seq
       mergeIntoHash content wc3
       mergeIntoHash content wc2
       mergeIntoHash content wc1

       a <- subseq content id_Begin id_End

       consoleLog $ map literal a

       push op_pool "first"
       push op_pool "second"
       x <- pop op_pool
       consoleLog x
       x <- pop op_pool
       consoleLog x
--       storeHash th "key" (8,3)
--       a <- readHash th "key"
--       x <- (getProp editor "innerHTML")
--       setProp editor "innerHTML" $ show x
       return ()


append ws str = withElems ["editor"] $
   \[editor] ->
   do  x<- getProp editor "innerHTML"
       setProp editor "innerHTML" (x ++ (fromJSStr str))

send ws = do wsSend ws $ toJSStr "roundtriptest"

hello :: Server (IORef a,IORef b) -> Server Int
hello state = return 17

-- | Launch the application!
main :: IO ()
main = do
  -- Run the Haste.App application. Please note that a computation in the App
  -- monad should never contain any free variables.
  runApp (mkConfig "ws://localhost:24601" 24601) $ do
    -- Create our state-holding elements
    state <- liftServerIO $ do
      clients <- newIORef []
      messages <- newIORef []
      return (clients, messages)

    -- Create an API object holding all available functions
    api <- API <$> remote (hello state)

    -- Launch the client
    runClient $ clientMain api

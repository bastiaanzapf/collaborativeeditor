
import Haste
import Haste.App
import Haste.App.Concurrent

import Haste.WebSockets

import JSHash
import Editor
import ConsoleLog
import Profile
import Server
import Operations
import Client

import Data.IORef
import Control.Applicative

append ws str = withElems ["editor"] $
   \[editor] ->
   do  x<- getProp editor "innerHTML"
       setProp editor "innerHTML" (x ++ (fromJSStr str))

-- | Launch the application!
main :: IO ()
main = do
  -- Run the Haste.App application. Please note that a computation in the App
  -- monad should never contain any free variables.
  runApp (mkConfig "ws://localhost:24601" 24601) $ do
    -- Create our state-holding elements
    state <- liftServerIO $ do
      clients <- newIORef 0
      messages <- newIORef []
      return (clients, messages)

    -- Create an API object holding all available functions
    api <- API <$> remote (hello state)
               <*> remote (send state)
               <*> remote (await state)

    -- Launch the client
    runClient $ clientMain api


module Server (State, API (API, apiHello, apiSend, apiAwait),
               hello, await, send) where

import Data.IORef

import Haste
import Haste.Prim
import Haste.App
import Haste.App.Concurrent

type State = (IORef Int, IORef [String])

data API = API {
    apiHello :: Remote (Server Int),
    apiSend :: Remote (Server () ),
    apiAwait :: Remote (Server () )
  }

hello :: Server State -> Server Int
hello state = do
  do (clients,_) <- state
     liftIO $ do 
       putStrLn "hello"
       count <- readIORef clients
       writeIORef clients (count+1)
       return (count+1)

await :: Server State -> Server ()
await _ = do liftIO $ putStrLn "await"
             return ()

send :: Server State -> Server ()
send _ = return ()

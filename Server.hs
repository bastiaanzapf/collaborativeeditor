
module Server (State, API (API, apiHello, apiSend, apiAwait),
               hello, await, send) where

import Data.IORef

import Haste
import Haste.Prim
import Haste.App
import Haste.App.Concurrent
import qualified Control.Concurrent as C
import Control.Monad
import Operations
import Data.Dequeue

type State = (IORef Int, IORef (BankersDequeue Operation))

data API = API {
    apiHello :: Remote ( Server Int             ),
    apiSend  :: Remote ( Operation -> Server () ),
    apiAwait :: Remote ( Server ()              )
  }

enqueue :: Operation ->  IORef (BankersDequeue Operation) -> IO ()
enqueue op ioref = atomicModifyIORef' ioref (\x -> (pushFront x op , ()))

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

send :: Server State -> Operation -> Server ()
send state op = do
  liftIO $ putStrLn "send"
  liftIO $ putStrLn $ show op
  (clients,messages) <- state
  return ()
--  msgarray <- liftIO $ readIORef messages
--  liftIO $ forM_ msgarray $ \x -> return ()



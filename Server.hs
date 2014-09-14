
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

type State = (IORef [(SessionID,C.MVar Operation)], 
              IORef (BankersDequeue Operation) )

data API = API {
    apiHello :: Remote ( Server Int             ),
    apiSend  :: Remote ( Operation -> Server () ),
    apiAwait :: Remote ( Server Operation       )
  }


newconnection :: SessionID ->
                 C.MVar Operation ->
                 [(SessionID,C.MVar Operation)] -> 
                 [(SessionID,C.MVar Operation)]
newconnection sid mvar list = (sid,mvar):list

hello :: Server State -> Server Int
hello state = do
  do (clients,_) <- state
     sid <- getSessionID
     liftIO $ do 
       putStrLn "hello"
       mvar <- C.newEmptyMVar
       atomicModifyIORef' clients (\x->(newconnection sid mvar x,()))
       return $ fromIntegral sid

await :: Server State -> Server Operation
await state = do 
  liftIO $ putStrLn "await"
  (clients,_) <- state
  clients' <- liftIO $ readIORef clients
  sid <- getSessionID
  let myvar = lookup sid clients'
  case myvar of
    Just mvar -> liftIO $ C.takeMVar mvar
    _ -> error "Session not found"

enqueue :: IORef (BankersDequeue Operation) -> Operation -> IO ()
enqueue ioref op = atomicModifyIORef' ioref (\x -> (pushFront x op , ()))

send :: Server State -> Operation -> Server ()
send state op = 
    do (clients,messages) <- state
       sender <- getSessionID
       liftIO $ do
         putStrLn "send"
         putStrLn $ show op
         enqueue messages op
         q <- readIORef messages
         putStrLn $ "dequeue length: " ++ show (Data.Dequeue.length q)
         clients' <- readIORef clients
         forM_ clients' $ \(_, v) -> C.forkIO $ C.putMVar v op




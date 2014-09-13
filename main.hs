
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

import Data.IORef
import Control.Applicative


--newtype Id = Mk_Id (Int,Int) deriving (Eq, Show, Read)

id_Begin = Mk_Id (0,0)
id_End   = Mk_Id (9999999,0)


wc_begin = W_Character {Editor.id=id_Begin,visible=False,literal=' ',previous_id=id_Begin,next_id=id_End}
wc_end = W_Character {Editor.id=id_End,visible=False,literal=' ',previous_id=id_Begin,next_id=id_End}

wc1 = W_Character {Editor.id=Mk_Id (1,1),visible=True,literal='a',previous_id=id_Begin,next_id=id_End}

wc2 = W_Character {Editor.id=Mk_Id (1,2),visible=True,literal='b',previous_id=id_Begin,next_id=id_End}

wc3 = W_Character {Editor.id=Mk_Id (1,4),visible=True,literal='c',previous_id=id_Begin,next_id=id_End}

showKey hash key = do x<-readHash hash key
                      consoleLog $ show x

testHash hash = do sequence $ map (showKey hash) [id_Begin,Mk_Id (1,1),Mk_Id (1,2),Mk_Id (1,4),id_End]
                   return ()

insertDummy = (Insert 
               W_Character { Editor.id = Mk_Id (-2,0) , 
                             literal = 'x' ,
                             visible = True,
                             next_id = Mk_Id (-2,0),
                             previous_id = Mk_Id (-2,0) } )

sendKey :: API -> Int -> Client ()
sendKey api k = do consoleLog "test"
                   case k of 
                     8 -> onServer $ apiSend api <.> (Delete $ Mk_Id (-3,0))
                     _ -> onServer $ apiSend api <.> (insertDummy)
                   consoleLog "test2"


test k = putStrLn "test"
clientMain :: API -> Client ()
clientMain api = withElems ["editor"] $ \[editor] -> do 
       setProp editor "contentEditable" "true"

       id <- onServer $ apiHello api
       consoleLog $ show id               

       Haste.App.onEvent editor OnKeyDown $ sendKey api

       setProp editor "innerHTML" "0123456789"

       content <- newHash "editor_data" :: Client (JSHash Id W_Character)
       op_pool <- newHash "pool" :: Client (JSHash Int String)

       let storeInContent x = storeHash content (Editor.id x) x

       storeInContent wc_begin
       storeInContent wc_end

       seq <- subseq content id_Begin id_End

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

       consoleLog "test"
       fork $ let awaitLoop test = do 
                    test <- onServer $ apiAwait api
                    return ()
                  in awaitLoop id

       return ()


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

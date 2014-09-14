
module Client (clientMain) where

import Haste
import Haste.Prim
import Haste.App
import Haste.App.Concurrent

import Haste.Foreign

import ConsoleLog
import JSHash
import Editor
import Operations
import Server

import Data.Char
import Data.IORef

id_Begin = Mk_Id (0,0)
id_End   = Mk_Id (9999999,0)

wc_begin = W_Character {Editor.id=id_Begin,visible=False,literal=' ',previous_id=id_Begin,next_id=id_End}
wc_end = W_Character {Editor.id=id_End,visible=False,literal=' ',previous_id=id_Begin,next_id=id_End}

wc1 = W_Character {Editor.id=Mk_Id (1,1),visible=True,literal='a',previous_id=id_Begin,next_id=id_End}

wc2 = W_Character {Editor.id=Mk_Id (1,2),visible=True,literal='b',previous_id=id_Begin,next_id=id_End}

wc3 = W_Character {Editor.id=Mk_Id (1,4),visible=True,literal='c',previous_id=id_Begin,next_id=id_End}


insertWChar id k = (Insert 
                   W_Character { Editor.id = id , 
                                 literal = chr k ,
                                 visible = True,
                                 next_id = Mk_Id (-2,0),
                                 previous_id = Mk_Id (-2,0) } )

increment k = (k+1,k)

sendKey :: API -> Int -> IORef Int -> Int -> Client ()
sendKey api station counter k = 
    do count <- liftIO $ atomicModifyIORef counter increment
       consoleLog "sendKey"
       consoleLog $ show k
       onServer $ apiSend api <.> insertWChar (Mk_Id (station,count)) k

newCounter :: Client (IORef Int)
newCounter = liftIO $ newIORef 0

initialize api editor = 
    do setProp editor "contentEditable" "true"
       id <- onServer $ apiHello api
       consoleLog $ "Connected to server, session id " ++ (show id)
       counter <- newCounter
       return (id,counter)

clientMain :: API -> Client ()
clientMain api = withElems ["editor"] $ \[editor] -> do       

       (sessionid,keycounter) <- initialize api editor

       let bindEditorKeypress = Haste.App.onEvent editor OnKeyPress

       bindEditorKeypress (sendKey api sessionid keycounter)

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
       fork $ let awaitLoop = do 
                    op <- onServer $ apiAwait api

                    consoleLog "message received"
                    case op of
                      Insert wchar -> do mergeIntoHash content wchar
                                         consoleLog $ show wchar
                      Delete id -> consoleLog "delete not implemented yet"
                    awaitLoop

                  in awaitLoop

       return ()


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
import Caret

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
                                 literal = k ,
                                 visible = True,
                                 next_id = Mk_Id (-2,0),
                                 previous_id = Mk_Id (-2,0) } )

increment k = (k+1,k)

sendKey :: API -> Int -> IORef Int -> Char -> Client ()
sendKey api station counter char = 
    do count <- liftIO $ atomicModifyIORef' counter increment
       consoleLog "sendKey"
       consoleLog $ show char
       onServer $ apiSend api <.> insertWChar (Mk_Id (station,count)) char

newCounter :: Client (IORef Int)
newCounter = liftIO $ newIORef 0

initialize api editor = 
    do setProp editor "contentEditable" "true"
       id <- onServer $ apiHello api
       consoleLog $ "Connected to server, session id " ++ (show id)
       counter <- newCounter
       caretposition <- newCounter
       return (id,counter,caretposition)

react :: Elem -> API -> IORef Int -> IORef Int -> Client ()
react editor api counter caretposition = do 
  p <- caretPosition editor
  op <- liftIO $ readIORef caretposition
  case p of
    Just p' -> do liftIO $ writeIORef caretposition p'
                  if (p' == op+1)
                  then do c <- characterLeftOfCaret editor 
                          case c of
                            Just c' ->  sendKey api 0 counter $ chr c'
                            Nothing ->  return ()
                  else return ()
    Nothing -> return ()

            
mouse :: Elem -> API -> IORef Int -> IORef Int -> Int -> (Int,Int) -> Client ()
mouse editor api counter caretposition k co = react editor api counter caretposition 

keyboard :: Elem -> API -> IORef Int -> IORef Int -> Int -> Client ()
keyboard editor api counter caretposition k = react editor api counter caretposition 

clientMain :: API -> Client ()
clientMain api = withElems ["editor"] $ \[editor] -> do       

       (sessionid,keycounter,caretposition) <- initialize api editor

       let bindEditorEvent response evt = Haste.App.onEvent editor evt response

       sequence_ $ map (bindEditorEvent (keyboard editor api keycounter caretposition))
           [OnKeyUp,OnKeyDown,OnKeyPress]

       sequence_ $ map (bindEditorEvent (mouse editor api keycounter caretposition))
               [OnClick,OnMouseUp,OnMouseDown]

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

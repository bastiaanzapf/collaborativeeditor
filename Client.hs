
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

data ClientState = ClientState { sentCounter   :: IORef Int ,
                                 caretPosition :: IORef Int ,
                                 contentLength :: IORef Int }

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

sendKey :: API -> Int -> ClientState -> Char -> Client ()
sendKey api station state char = 
    do let counter = sentCounter state
       count <- liftIO $ atomicModifyIORef' counter increment
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
       clcounter <- newCounter
       return $ ClientState { sentCounter=counter, 
                              Client.caretPosition=caretposition,
                              contentLength=clcounter }

data EventType = Character
               | Clipboard
               | Other

react :: Elem -> API -> ClientState -> Client ()
react editor api state = do 
  editorPosition <- Caret.caretPosition editor :: Client (Maybe Int)
  let clientPosition = Client.caretPosition state
  oldEditorPosition <- liftIO $ readIORef $ clientPosition :: Client (Int)
  l <- textLength editor
  let textLength = contentLength state
  oldTextLength <- liftIO $ readIORef $ textLength
  case editorPosition of
    Just p' -> do liftIO $ writeIORef clientPosition p'
                  liftIO $ writeIORef textLength l
                  if (p' == oldEditorPosition + 1 &&
                      l  == oldTextLength +1 )
                  then do c <- characterLeftOfCaret editor 
                          case c of
                            Just c' ->  sendKey api 0 state $ chr c'
                            Nothing ->  return ()
                  else return ()
    Nothing -> return ()

            
mouse :: Elem -> API -> ClientState -> Int -> (Int,Int) -> Client ()
mouse editor api state k co = react editor api state

keyboard :: Elem -> API -> ClientState -> Int -> Client ()
keyboard editor api state k = react editor api state

clientMain :: API -> Client ()
clientMain api = withElems ["editor"] $ \[editor] -> do       

       clientstate <- initialize api editor

       let bindEditorEvent response evt = Haste.App.onEvent editor evt response

       sequence_ $ map (bindEditorEvent (keyboard editor api clientstate))
           [OnKeyUp,OnKeyDown,OnKeyPress]

       sequence_ $ map (bindEditorEvent (mouse editor api clientstate))
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

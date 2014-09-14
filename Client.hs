
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
                                 contentLength :: IORef Int ,
                                 contentHash   :: IORef (JSHash Id W_Character)}

id_Begin = Mk_Id (0,0)
id_End   = Mk_Id (9999999,0)

wc_begin = W_Character {Editor.id=id_Begin,Editor.visible=False,literal=' ',previous_id=id_Begin,next_id=id_End}
wc_end = W_Character {Editor.id=id_End,Editor.visible=False,literal=' ',previous_id=id_Begin,next_id=id_End}

wc1 = W_Character {Editor.id=Mk_Id (1,1),Editor.visible=True,literal='a',previous_id=id_Begin,next_id=id_End}

wc2 = W_Character {Editor.id=Mk_Id (1,2),Editor.visible=True,literal='b',previous_id=id_Begin,next_id=id_End}

wc3 = W_Character {Editor.id=Mk_Id (1,4),Editor.visible=True,literal='c',previous_id=id_Begin,next_id=id_End}


insertWChar id k previous next = 
  (Insert 
     W_Character { Editor.id = id , 
                   literal = k ,
                   Editor.visible = True,
                   next_id = next,
                   previous_id = previous } )

increment k = (k+1,k)

sendKey :: API -> Int -> ClientState -> Char -> W_Character -> W_Character -> Client ()
sendKey api station state char previous next = 
    do let counter = sentCounter state
       count <- liftIO $ atomicModifyIORef' counter increment
       consoleLog "sendKey"
       consoleLog $ show char

       let insert = insertWChar (Mk_Id (station,count)) char
                    (Editor.id previous) (Editor.id next)

       onServer $ apiSend api <.> insert

newCounter :: Client (IORef Int)
newCounter = liftIO $ newIORef 0

initialize api editor hash = 
    do setProp editor "contentEditable" "true"
       id <- onServer $ apiHello api
       consoleLog $ "Connected to server, session id " ++ (show id)
       counter <- newCounter
       caretposition <- newCounter
       clcounter <- newCounter
       return $ ClientState { sentCounter=counter, 
                              Client.caretPosition=caretposition,
                              contentLength=clcounter,
                              contentHash=hash }

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
                  hash <- liftIO $ readIORef $ contentHash state
                  previous <- visibleAt hash (oldEditorPosition-1)
                  next <- visibleAt hash (oldEditorPosition+1)
                  consoleLog $ show previous
                  consoleLog $ show next
                  case (previous,next) of
                    (Just previous,Just next) -> 
                        if (p' == oldEditorPosition + 1 &&
                            l  == oldTextLength +1 )
                        then do c <- characterLeftOfCaret editor 
                                case c of
                                  Just c' ->  sendKey api 0 
                                              state (chr c') 
                                              previous next
                                  Nothing ->  return ()
                        else return ()
                    _ -> error "visibleAt returned Nothing in react"
    Nothing -> return ()

mouse :: Elem -> API -> ClientState -> Int -> (Int,Int) -> Client ()
mouse editor api state k co = react editor api state

keyboard :: Elem -> API -> ClientState -> Int -> Client ()
keyboard editor api state k = react editor api state

visible' :: JSHash Id W_Character -> Id -> Client [Char]
visible' hash id = do 
  if id == id_End
  then return []
  else do x <- readHash hash id
          case x of
            Just wchar -> do tail <- visible' hash (next_id wchar)
                             if Editor.visible wchar
                             then return $ literal wchar : tail
                             else return $ tail
            Nothing    -> error $ "Did not find id " ++ 
                          show id ++
                          " in hash."
                      

visible :: JSHash Id W_Character -> Client [Char]
visible hash = visible' hash id_Begin

visibleAt' hash id position = do 
  if id == id_End
  then return $ Just wc_end
  else if position == 0
       then readHash hash id
       else do 
         consoleLog $ show position
         consoleLog $ show id
         x <- readHash hash id
         case x of
           Just wchar -> do if Editor.visible wchar
                            then visibleAt' hash (next_id wchar) 
                                 (position-1)
                            else visibleAt' hash (next_id wchar) 
                                 position
           Nothing    -> error $ "Did not find id " ++ 
                         show id ++
                        " in hash (visibleAt)."

visibleAt :: JSHash Id W_Character -> Int -> Client (Maybe W_Character)
visibleAt hash position = visibleAt' hash id_Begin position


clientMain :: API -> Client ()
clientMain api = withElems ["editor"] $ \[editor] -> do       

       content <- newHash "editor_data" :: Client (JSHash Id W_Character)
       op_pool <- newHash "pool" :: Client (JSHash Int String)

       contentIORef <- liftIO $ newIORef content

       clientstate <- initialize api editor contentIORef

       let bindEditorEvent response evt = Haste.App.onEvent editor evt response

       sequence_ $ map (bindEditorEvent (keyboard editor api clientstate))
           [OnKeyUp,OnKeyDown,OnKeyPress]

       sequence_ $ map (bindEditorEvent (mouse editor api clientstate))
               [OnClick,OnMouseUp,OnMouseDown]


       let storeInContent x = storeHash content (Editor.id x) x
                              
       storeInContent wc_begin
       storeInContent wc_end

       seq <- subseq content id_Begin id_End

       mergeIntoHash content wc3
       mergeIntoHash content wc2
       mergeIntoHash content wc1

       a <- subseq content id_Begin id_End

       initialcontent <- Client.visible content
       setProp editor "innerHTML" initialcontent

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
                    consoleLog $ show op
                    case op of
                      Insert wchar -> do mergeIntoHash content wchar
                                         consoleLog $ show wchar
                      Delete id -> consoleLog "delete not implemented yet"
                    awaitLoop

                  in awaitLoop

       return ()

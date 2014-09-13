
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

id_Begin = Mk_Id (0,0)
id_End   = Mk_Id (9999999,0)

wc_begin = W_Character {Editor.id=id_Begin,visible=False,literal=' ',previous_id=id_Begin,next_id=id_End}
wc_end = W_Character {Editor.id=id_End,visible=False,literal=' ',previous_id=id_Begin,next_id=id_End}

wc1 = W_Character {Editor.id=Mk_Id (1,1),visible=True,literal='a',previous_id=id_Begin,next_id=id_End}

wc2 = W_Character {Editor.id=Mk_Id (1,2),visible=True,literal='b',previous_id=id_Begin,next_id=id_End}

wc3 = W_Character {Editor.id=Mk_Id (1,4),visible=True,literal='c',previous_id=id_Begin,next_id=id_End}


insertDummy k = (Insert 
                   W_Character { Editor.id = Mk_Id (-2,0) , 
                                 literal = chr k ,
                                 visible = True,
                                 next_id = Mk_Id (-2,0),
                                 previous_id = Mk_Id (-2,0) } )

sendKey :: API -> Int -> Client ()
sendKey api k = do consoleLog "sendKey"
                   consoleLog $ show k
                   onServer $ apiSend api <.> insertDummy k

clientMain :: API -> Client ()
clientMain api = withElems ["editor"] $ \[editor] -> do 
       setProp editor "contentEditable" "true"

       id <- onServer $ apiHello api
       consoleLog $ show id

       Haste.App.onEvent editor OnKeyPress $ sendKey api

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

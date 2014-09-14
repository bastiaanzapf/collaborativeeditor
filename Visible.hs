module Visible (Visible.visible,visibleAt) where

import Haste
import Haste.App
import WCharacter
import JSHash
import ConsoleLog
import Editor

id_Begin = Mk_Id (0,0)
id_End   = Mk_Id (9999999,0)

wc_begin = W_Character {Editor.id=id_Begin,Editor.visible=False,literal=' ',previous_id=id_Begin,next_id=id_End}
wc_end = W_Character {Editor.id=id_End,Editor.visible=False,literal=' ',previous_id=id_Begin,next_id=id_End}

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
  then if position == 0 
       then return $ Just wc_end
       else error "Hash exceeded in visibleAt"
  else if position == 0
       then readHash hash id
       else do 
--         consoleLog $ show position
--         consoleLog $ show id
         x <- readHash hash id
         case x of
           Just wchar -> do if Editor.visible wchar ||
                               id == id_Begin
                            then visibleAt' hash (next_id wchar)
                                 (position-1)
                            else visibleAt' hash (next_id wchar) 
                                 position
           Nothing    -> error $ "Did not find id " ++ 
                         show id ++
                        " in hash (visibleAt)."

visibleAt :: JSHash Id W_Character -> Int -> Client (Maybe W_Character)
visibleAt hash position = 
    if position >= 0
    then visibleAt' hash id_Begin position
    else if position == -1
         then return $ Just wc_begin
         else return Nothing

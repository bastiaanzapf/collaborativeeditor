
module Editor (Id(Mk_Id),W_Character(W_Character),
               WCharacter.id,visible,literal,previous_id,next_id,  
               subseq,mergeIntoHash) where

import Haste.Prim
import Haste.Foreign
import Haste.App
import JSHash
import ConsoleLog
import WCharacter

data Operation = Insert Id Char Id Id
               | Delete Id

operation_to_wchar (Insert a b c d) = 
    W_Character { WCharacter.id=a, visible=True, literal=b, previous_id=c, next_id=d }

subseq :: (JSHash Id W_Character) -> Id -> Id -> Client ( [ W_Character ])
subseq hash previous next = do 
  if previous == next 
  then return []
  else do hd <- readHash hash previous
          case hd of
            Just hd' -> do tl <- (subseq hash (next_id hd') next)
                           return (hd':tl)
            Nothing  -> error "Wchar not found in subseq"

insert' hash previous next wchar = do
  let storeWithId x = storeHash hash (WCharacter.id x) x

  storeWithId $ W_Character {WCharacter.id=WCharacter.id previous,
                             visible=visible previous,
                             literal=literal previous,
                             previous_id=previous_id previous,
                             next_id=WCharacter.id wchar}

  storeWithId wchar

  storeWithId $ W_Character {WCharacter.id=WCharacter.id next,
                             visible=visible next,
                             literal=literal next,
                             previous_id=WCharacter.id wchar,
                             next_id=next_id next}

insert hash previous_id next_id wchar =
    do previous <- readHash hash $ previous_id wchar
       next     <- readHash hash $ next_id     wchar

       case (previous,next) of
         (Just previous, Just next) -> do insert' hash previous next wchar
         _ -> error "WCharacter not found in insert"




findPosition (hwc:hwc2:twc) wc = if (WCharacter.id wc)<=(WCharacter.id hwc2)
                                    then (hwc,hwc2)
                                    else findPosition (hwc2:twc) wc

findPosition (hwc:[]) _ = error "Could not find position"
findPosition [] _ = error "Could not find position"

between wc1 wc2 wc = W_Character {WCharacter.id=WCharacter.id wc,
                                  visible=visible wc,
                                  literal=literal wc,
                                  previous_id=WCharacter.id wc1,
                                  next_id=WCharacter.id wc2}

mergeIntoHash hash wchar = 
    do seq <- subseq hash (previous_id wchar) (next_id wchar)
       if seq == [] || tail seq == [] 
       then insert hash previous_id next_id wchar
       else do next <- readHash hash (next_id wchar)
               case next of
                 Just a -> do let inclseq = seq ++ [ a ]
                              let (a,b) = findPosition inclseq wchar
                              mergeIntoHash hash (between a b wchar)

                 Nothing -> error "mergeIntoHash: next wchar not found"


module Editor (Id(Mk_Id),W_Character(W_Character),
               Editor.id,visible,literal,previous_id,next_id,  
               subseq,mergeIntoHash) where

import Haste.Prim
import Haste.Foreign
import Haste.App
import JSHash
import ConsoleLog

data Id = Mk_Id (Int,Int) deriving (Show, Read, Eq, Ord)

data W_Character = W_Character { id          :: Id
                               , visible     :: Bool
                               , literal     :: Char
                               , previous_id :: Id
                               , next_id     :: Id    } 
                   deriving (Show, Read, Eq)

instance Pack W_Character where

instance Unpack W_Character where

data Operation = Insert Id Char Id Id
               | Delete Id

operation_to_wchar (Insert a b c d) = 
    W_Character { Editor.id=a, visible=True, literal=b, previous_id=c, next_id=d }

subseq :: (JSHash Id W_Character) -> Id -> Id -> Client [ W_Character ]
subseq hash previous next = do if previous == next 
                               then return []
                               else do hd <- readHash hash previous
                                       tl <- (subseq hash (next_id hd) next)
                                       return (hd:tl)

insert hash previous_id next_id wchar =
    do previous <- readHash hash $ previous_id wchar
       next     <- readHash hash $ next_id     wchar

       let storeWithId x = storeHash hash (Editor.id x) x

       storeWithId $ W_Character {Editor.id=Editor.id previous,
                                  visible=visible previous,
                                  literal=literal previous,
                                  previous_id=previous_id previous,
                                  next_id=Editor.id wchar}

       storeWithId wchar

       storeWithId $ W_Character {Editor.id=Editor.id next,
                                  visible=visible next,
                                  literal=literal next,
                                  previous_id=Editor.id wchar,
                                  next_id=next_id next}

findPosition (hwc:hwc2:twc) wc = if (Editor.id wc)<=(Editor.id hwc2)
                                    then (hwc,hwc2)
                                    else findPosition (hwc2:twc) wc

findPosition (hwc:[]) _ = error "Could not find position"
findPosition [] _ = error "Could not find position"

between wc1 wc2 wc = W_Character {Editor.id=Editor.id wc,
                                  visible=visible wc,
                                  literal=literal wc,
                                  previous_id=Editor.id wc1,
                                  next_id=Editor.id wc2}

mergeIntoHash hash wchar = 
    do seq <- subseq hash (previous_id wchar) (next_id wchar)
       if tail seq == []
       then insert hash previous_id next_id wchar
       else do next <- readHash hash (next_id wchar)               
               let inclseq = seq ++ [ next ]
               let (a,b) = findPosition inclseq wchar
               mergeIntoHash hash (between a b wchar)

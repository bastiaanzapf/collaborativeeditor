
module Editor (Id(Mk_Id),W_Character(W_Character),
               WCharacter.id,visible,literal,previous_id,next_id,  
               subseq,mergeIntoHash) where

import Haste.Prim
import Haste.Foreign
import Haste.App
import JSHash
import ConsoleLog
import WCharacter

subseq :: (JSHash Id W_Character) -> Id -> Id -> Client ( [ W_Character ])
subseq hash previous next = do 
  if previous == next 
  then return []
  else do hd <- readHash hash previous
          case hd of
            Just hd' -> do tl <- (subseq hash (next_id hd') next)
                           return (hd':tl)
            Nothing  -> error "Wchar not found in subseq"

id_Begin = Mk_Id (0,0)
id_End   = Mk_Id (9999999,0)

insert' :: (JSHash Id W_Character) -> W_Character -> W_Character -> W_Character -> Client ()
insert' hash previous next wchar = do
  let storeWithId x = storeHash hash (WCharacter.id x) x

  if (next_id previous == (WCharacter.id next)) &&
     (previous_id next == (WCharacter.id previous))
  then return ()
  else error $ "Can only insert between two characters, instead: " ++ (show previous) ++ " " ++ (show next)

  x <- readHash hash (WCharacter.id wchar)

  case x of
    Just x -> error $ "Existing character inserted: " ++ (show $ WCharacter.id x)
    _      -> return ()

  storeWithId $ W_Character {WCharacter.id=(WCharacter.id previous),
                             visible=visible previous,
                             literal=literal previous,
                             previous_id=previous_id previous,
                             next_id=WCharacter.id wchar}

  storeWithId $ W_Character {WCharacter.id=(WCharacter.id wchar),
                             visible=visible wchar,
                             literal=literal wchar,
                             previous_id=WCharacter.id previous,
                             next_id=WCharacter.id next}

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

findPosition (hwc:hwc2:twc) wc = if (WCharacter.id wc)<=(WCharacter.id hwc2) ||
                                    twc == []
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
    do consoleLog $ "merge "++(show wchar)
       consoleLog $ "Not (0,0)" ++ (show $ WCharacter.id wchar)
       if (WCharacter.id wchar == Mk_Id (0,0))
       then error "Merge Character (0,0)"
       else return ()
       a <- readHash hash (previous_id wchar)
       b <- readHash hash (next_id wchar)
       case a of
         Just _ -> return ()
         _ -> liftIO $ putStrLn $ "previous id " ++ (show $ previous_id wchar) ++ " not found"
       case b of
         Just _ -> return ()
         _ -> liftIO $ putStrLn $ "next id " ++ (show $ next_id wchar) ++ " not found"

       case (a,b) of
         (Just _,Just _) -> return ()
         _ -> error "abort"

       seq <- subseq hash (previous_id wchar) (next_id wchar)
       consoleLog $ show seq
       if seq == [] || tail seq == [] 
       then insert hash previous_id next_id wchar
       else do next <- readHash hash (next_id wchar)
               case next of
                 Just a -> do let inclseq = seq ++ [ a ]
                              let (a,b) = findPosition inclseq wchar
                              consoleLog $ "findPosition: " ++ show (a,b)
                              mergeIntoHash hash (between a b wchar)

                 Nothing -> error "mergeIntoHash: next wchar not found"

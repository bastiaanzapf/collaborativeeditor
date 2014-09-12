
import Haste
import Haste.Prim
import Haste.App
--import Haste.App.Concurrent
import Haste.Foreign

import Haste.WebSockets

import JSHash

--newtype Id = Mk_Id (Int,Int) deriving (Eq, Show, Read)

data Id = Mk_Id (Int,Int) deriving (Show, Read, Eq, Ord)

id_Begin = Mk_Id (0,0)
id_End   = Mk_Id (9999999,0)

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
    W_Character { Main.id=a, visible=True, literal=b, previous_id=c, next_id=d }

jsEscape ('\'':tc) = '\\':'\'':jsEscape tc
jsEscape (x:tc) = x:jsEscape tc
jsEscape [] = []

consoleLog :: String -> IO ()
consoleLog str = 
    ffi $ toJSStr ("console.log('" ++ jsEscape str ++ "');")

newIntegerArray :: String -> IO (JSHash Int a)
newIntegerArray name = newHash name

subseq :: (JSHash Id W_Character) -> Id -> Id -> IO [ W_Character ]
subseq hash previous next = do if previous == next 
                               then return []
                               else do hd <- readHash hash previous
                                       tl <- (subseq hash (next_id hd) next)
                                       return (hd:tl)

wc_begin = W_Character {Main.id=id_Begin,visible=False,literal=' ',previous_id=id_Begin,next_id=id_End}
wc_end = W_Character {Main.id=id_End,visible=False,literal=' ',previous_id=id_End,next_id=id_Begin}

wc1 = W_Character {Main.id=Mk_Id (1,1),visible=True,literal='a',previous_id=id_Begin,next_id=id_End}

wc2 = W_Character {Main.id=Mk_Id (1,2),visible=True,literal='b',previous_id=id_Begin,next_id=id_End}

wc3 = W_Character {Main.id=Mk_Id (1,4),visible=True,literal='c',previous_id=id_Begin,next_id=id_End}

insert hash previous_id next_id wchar =
    do previous <- readHash hash $ previous_id wchar
       next     <- readHash hash $ next_id     wchar

       let storeWithId x = storeHash hash (Main.id x) x

       storeWithId $ W_Character {Main.id=Main.id previous,
                                  visible=visible previous,
                                  literal=literal previous,
                                  previous_id=previous_id previous,
                                  next_id=Main.id wchar}

       storeWithId wchar

       storeWithId $ W_Character {Main.id=Main.id next,
                                  visible=visible next,
                                  literal=literal next,
                                  previous_id=Main.id wchar,
                                  next_id=next_id next}

findPosition (hwc:hwc2:twc) wc = if (Main.id wc)<=(Main.id hwc2)
                                    then (hwc,hwc2)
                                    else findPosition (hwc2:twc) wc

findPosition (hwc:[]) _ = error "Could not find position"
findPosition [] _ = error "Could not find position"

between wc1 wc2 wc = W_Character {Main.id=Main.id wc,
                                  visible=visible wc,
                                  literal=literal wc,
                                  previous_id=Main.id wc1,
                                  next_id=Main.id wc2}

mergeIntoHash hash wchar = 
    do seq <- subseq hash (previous_id wchar) (next_id wchar)
       if tail seq == []
       then insert hash previous_id next_id wchar
       else do next <- readHash hash (next_id wchar)               
               let inclseq = seq ++ [ next ]
               let (a,b) = findPosition inclseq wchar
               mergeIntoHash hash (between a b wchar)

clientMain :: IO ()
clientMain = withElems ["editor"] $ 
    \[editor] ->
    do setProp editor "innerHTML" "0123456789"
       content <- newHash "content" :: IO (JSHash Id W_Character)
       op_pool <- newIntegerArray "pool" :: IO (JSHash Int String)

       let storeInContent x = storeHash content (Main.id x) x

       storeInContent wc_begin
       storeInContent wc_end

       seq <- subseq content id_Begin id_End
--       consoleLog $ show $ tail seq
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
--       storeHash th "key" (8,3)
--       a <- readHash th "key"
--       x <- (getProp editor "innerHTML")
--       setProp editor "innerHTML" $ show x
       return ()


append ws str = withElems ["editor"] $
   \[editor] ->
   do  x<- getProp editor "innerHTML"
       setProp editor "innerHTML" (x ++ (fromJSStr str))

send ws = do wsSend ws $ toJSStr "roundtriptest"

-- | Launch the application!
main :: IO ()
main = concurrent $ do x<-newEmptyMVar
                       liftIO $ clientMain
--                       withWebSocket "ws://localhost:9000/ws" append (return ()) send
                       return ()



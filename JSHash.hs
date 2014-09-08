
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances    #-}

module JSHash (JSHash, newHash, storeHash, readHash, push, pop) where

import Haste.Prim
import Haste.Foreign

newtype JSHash a b = Mknt String

class Hashable a where
    hashKey :: a -> String

instance Show x => Hashable x where
    hashKey = show

newHash :: String -> IO (JSHash key a)
newHash str = do (ffi $ toJSStr (str ++ "=Array();"))::IO ()
                 return $ Mknt str

storeHash :: (Hashable key,Show a) => (JSHash key a) -> key -> a -> IO ()
storeHash (Mknt hash) key value = 
       ffi $ toJSStr 
            (hash ++ "['" ++ (hashKey key) ++ "']='" ++ (show value) ++ "'")

consoleLog :: String -> IO ()
consoleLog str = ffi $ toJSStr ("console.log('" ++ str ++ "');")

readHash :: (Hashable key, Unpack a,Pack a, Read a) => (JSHash key (Ptr a)) -> key -> IO a
readHash (Mknt hash) key = do x <- ffi $ toJSStr (hash ++ "['" ++ (hashKey key) ++ "']")
                              return $ read x

push :: (Show b) => JSHash Int b -> b -> IO () 
push (Mknt hash) value = ffi $ toJSStr $ hash ++ ".push('" ++ (show value) ++ "')"

pop :: (Pack b, Unpack b) => JSHash Int b -> IO b 
pop (Mknt hash) = ffi $ toJSStr $ hash ++ ".pop()"

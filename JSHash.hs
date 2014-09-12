
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE FlexibleContexts     #-}

module JSHash (JSHash, newHash, storeHash, readHash, push, pop) where

import Haste.Prim
import Haste.Foreign
import Haste.App
import ConsoleLog
import JSEscape

newtype JSHash a b = Mknt String deriving (Show,Read)

class Hashable a where
    hashKey :: a -> String

instance Show x => Hashable x where
    hashKey = show

newHash :: String -> Client (JSHash key a)
newHash str = do let js = ("window." ++ (str) ++ "=Array()") :: String
                 liftIO $ ffi $ toJSStr js :: Client ()
                 return $ Mknt str


storeHash :: (Hashable key,Show key, Show a) => (JSHash key a) -> key -> a -> Client ()
storeHash (Mknt hash) key value = do
  consoleLog "storeHash"
  consoleLog $ hashKey key
  let js = ("window." ++ hash ++ "['" ++ hashKey key ++ "']=" ++ "'" ++(jsEscape $ show value) ++ "'")
  consoleLog $ show js
  liftIO $ ffi $ toJSStr js

readHash :: (Hashable key, Unpack a,Pack a, Read a,Show a,Show key ) => 
            (JSHash key a) -> key -> Client a
readHash (Mknt hash) key = do 
  consoleLog "readHash"
  consoleLog $ hashKey key
  let js = ("window." ++ hash ++ "['" ++ hashKey key ++ "']")
  x <- liftIO $ ffi $ toJSStr js
  consoleLog $ x
  return $ read x

push :: (Show b) => JSHash Int b -> b -> Client () 
push (Mknt hash) value = do
  let js = ("window." ++ hash ++ ".push('" ++ (jsEscape $ show value) ++ "')")
  liftIO $ ffi $ toJSStr js :: Client ()

pop :: (Pack b, Unpack b) => JSHash Int b -> Client b
pop (Mknt hash) = do
  let js = ("window." ++ hash ++ ".pop()")
  liftIO $ ffi $ toJSStr js 


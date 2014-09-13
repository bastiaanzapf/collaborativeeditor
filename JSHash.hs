
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE FlexibleContexts     #-}

module JSHash (JSHash, newHash, storeHash, readHash, push, pop) where

import Haste.Prim
import Haste.Foreign
import Haste.App
import ConsoleLog
import JSEscape

foreign import ccall jsNewHash :: Ptr JSString -> IO ()
foreign import ccall jsStoreHash :: Ptr JSString -> Ptr JSString -> Ptr JSString -> IO ()

newtype JSHash a b = Mknt String deriving (Show,Read)

class Hashable a where
    hashKey :: a -> String

instance Show x => Hashable x where
    hashKey = jsEscape . show

newHash :: String -> Client (JSHash key a)
newHash str = do 
  let hashName = toPtr $ toJSStr str
  liftIO $ jsNewHash hashName
  return $ Mknt str


storeHash :: (Hashable key,Show key, Show a) => (JSHash key a) -> key -> a -> Client ()
storeHash (Mknt hash) key value = do
  let hashName = toPtr $ toJSStr hash
  let hashKey' = toPtr $ toJSStr $ hashKey key
  let value'   = toPtr $ toJSStr $ show value
  liftIO $ jsStoreHash hashName hashKey' value'

readHash :: (Hashable key, Unpack a,Pack a, Read a,Show a,Show key ) => 
            (JSHash key a) -> key -> Client a
readHash (Mknt hash) key = do 
  let js = ("window." ++ hash ++ "['" ++ hashKey key ++ "']")
  x <- liftIO $ ffi $ toJSStr js
  return $ read x

push :: (Show b) => JSHash Int b -> b -> Client () 
push (Mknt hash) value = do
  let js = ("window." ++ hash ++ ".push('" ++ (jsEscape $ show value) ++ "')")
  liftIO $ ffi $ toJSStr js :: Client ()

pop :: (Pack b, Unpack b) => JSHash Int b -> Client b
pop (Mknt hash) = do
  let js = ("window." ++ hash ++ ".pop()")
  liftIO $ ffi $ toJSStr js 


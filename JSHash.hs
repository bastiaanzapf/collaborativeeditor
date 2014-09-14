
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE CPP #-}

module JSHash (JSHash, newHash, storeHash, readHash, push, pop) where

import Haste.Prim
import Haste.Foreign
import Haste.App
import ConsoleLog
import JSEscape

#ifdef __HASTE__

foreign import ccall jsNewHash :: JSString -> IO ()
foreign import ccall jsStoreHash :: JSString -> JSString -> Ptr a -> IO ()
foreign import ccall jsReadHash :: JSString -> JSString -> IO (Ptr a)

#else

jsNewHash :: JSString -> IO ()
jsNewHash = error "jsNewHash called on server side"

jsStoreHash :: JSString -> JSString -> Ptr a -> IO ()
jsStoreHash = error "jsStoreHash called on server side"

jsReadHash :: JSString -> JSString -> IO (Ptr a)
jsReadHash = error "jsReadHash called on server side"

#endif

newtype JSHash a b = Mknt String deriving (Show,Read)

class Hashable a where
    hashKey :: a -> String

instance Show x => Hashable x where
    hashKey = jsEscape . show

newHash :: String -> Client (JSHash key a)
newHash str = do 
  let hashName = toJSStr str
  liftIO $ jsNewHash hashName
  return $ Mknt str


storeHash :: (Hashable key,Show key, Show a) => (JSHash key a) -> key -> a -> Client ()
storeHash (Mknt hash) key value = do
  let hashName = toJSStr hash
  let hashKey' = toJSStr $ hashKey key
  liftIO $ jsStoreHash hashName hashKey' $ toPtr value

readHash :: (Hashable key, Unpack a,Pack a, Read a,Show a,Show key ) => 
            (JSHash key a) -> key -> Client a
readHash (Mknt hash) key = do 
  let hashName = toJSStr hash
  let hashKey' = toJSStr $ hashKey key
  x <- liftIO $ jsReadHash hashName hashKey'
  return $ fromPtr x

push :: (Show b) => JSHash Int b -> b -> Client () 
push (Mknt hash) value = do
  let js = ("window." ++ hash ++ ".push('" ++ (jsEscape $ show value) ++ "')")
  liftIO $ ffi $ toJSStr js :: Client ()

pop :: (Pack b, Unpack b) => JSHash Int b -> Client b
pop (Mknt hash) = do
  let js = ("window." ++ hash ++ ".pop()")
  liftIO $ ffi $ toJSStr js 


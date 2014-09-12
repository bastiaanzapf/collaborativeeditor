
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances    #-}

module JSHash (JSHash, newHash, storeHash, readHash, push, pop) where

import Haste.Prim
import Haste.Foreign

newtype JSHash a b = Mknt String deriving (Show,Read)

class Hashable a where
    hashKey :: a -> String

instance Show x => Hashable x where
    hashKey = show

foreign import ccall jsNewHash :: JSString -> IO ()
foreign import ccall jsStoreHash :: JSString -> JSString -> Ptr a -> IO ()
foreign import ccall jsReadHash :: JSString -> JSString -> IO (Ptr a)
foreign import ccall jsPush :: JSString -> Ptr a -> IO ()
foreign import ccall jsPop :: JSString -> IO (Ptr a)

newHash :: String -> IO (JSHash key a)
newHash str = do jsNewHash $ toJSStr str
                 return $ Mknt str


storeHash :: (Hashable key,Show a) => (JSHash key a) -> key -> a -> IO ()
storeHash (Mknt hash) key value = 
    jsStoreHash (toJSStr hash) (toJSStr $ hashKey key) (toPtr value)

consoleLog :: String -> IO ()
consoleLog str = ffi $ toJSStr ("console.log('" ++ str ++ "');")

readHash :: (Hashable key, Unpack a,Pack a, Read a,Show a ) => (JSHash key a) -> key -> IO a
readHash (Mknt hash) key = do x <- jsReadHash (toJSStr hash) (toJSStr $ hashKey key)
                              return $ fromPtr x

push :: (Show b) => JSHash Int b -> b -> IO () 
push (Mknt hash) value = jsPush (toJSStr hash) (toPtr value)

pop :: (Pack b, Unpack b) => JSHash Int b -> IO b 
pop (Mknt hash) = do x <- jsPop (toJSStr hash)
                     return $ fromPtr x

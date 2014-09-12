
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances    #-}

module JSHash (JSHash, newHash, storeHash, readHash, push, pop) where

import Haste.Prim
import Haste.Foreign
import Haste.App

newtype JSHash a b = Mknt String deriving (Show,Read)

class Hashable a where
    hashKey :: a -> String

instance Show x => Hashable x where
    hashKey = show

foreign import ccall jsNewHash :: Ptr JSString -> IO ()
foreign import ccall jsStoreHash :: Ptr JSString -> Ptr JSString -> Ptr a -> IO ()
foreign import ccall jsReadHash :: Ptr JSString -> Ptr JSString -> IO (Ptr a)
foreign import ccall jsPush :: Ptr JSString -> Ptr a -> IO ()
foreign import ccall jsPop :: Ptr JSString -> IO (Ptr a)

newHash :: String -> Client (JSHash key a)
newHash str = do liftIO $ jsNewHash $ toPtr $ toJSStr str
                 return $ Mknt str


storeHash :: (Hashable key,Show a) => (JSHash key a) -> key -> a -> Client ()
storeHash (Mknt hash) key value = liftIO $ 
    jsStoreHash (toPtr $ toJSStr hash) (toPtr $ toJSStr $ hashKey key) (toPtr value)

readHash :: (Hashable key, Unpack a,Pack a, Read a,Show a ) => (JSHash key a) -> key -> Client a
readHash (Mknt hash) key = do x <- liftIO $ jsReadHash (toPtr $ toJSStr hash) (toPtr $ toJSStr $ hashKey key)
                              return $ fromPtr x

push :: (Show b) => JSHash Int b -> b -> Client () 
push (Mknt hash) value = liftIO $ jsPush (toPtr $ toJSStr hash) (toPtr value)

pop :: (Pack b, Unpack b) => JSHash Int b -> Client b 
pop (Mknt hash) = do x <- liftIO $ jsPop (toPtr $ toJSStr hash)
                     return $ fromPtr x

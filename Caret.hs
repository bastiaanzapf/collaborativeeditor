
{-# LANGUAGE CPP #-}

module Caret ( caretPosition, characterLeftOfCaret, textLength,
               textInsertAt ) where

import Haste
import Haste.Prim
import Haste.App
import Haste.App.Concurrent

import Haste.Foreign

#ifdef __HASTE__

foreign import ccall jsCaretPosition :: Elem -> IO (Ptr (Maybe Int))
foreign import ccall jsCharacterLeftOfCaret :: Elem -> IO (Ptr (Maybe Int))
foreign import ccall jsTextLength :: Elem -> IO Int
foreign import ccall jsInsertAt :: Elem -> Int -> Char -> IO ()

#else

jsCaretPosition :: Elem -> IO (Ptr (Maybe Int))
jsCaretPosition = error "jsCaretPosition called on server side"

jsCharacterLeftOfCaret :: Elem -> IO (Ptr (Maybe Int))
jsCharacterLeftOfCaret = error "jsCharacterLeftOfCaret called on server side"

jsTextLength :: Elem -> IO Int
jsTextLength = error "jsTextLength called on server side"

jsInsertAt :: Elem -> Int -> Char -> IO Int
jsInsertAt = error "jsInsertAt called on server side"

#endif

caretPosition elem = do x <- liftIO $ jsCaretPosition elem
                        return $ fromPtr x

characterLeftOfCaret elem = do x <- liftIO $ jsCharacterLeftOfCaret elem
                               return $ fromPtr x

textLength elem = do x <- liftIO $ jsTextLength elem
                     return x

textInsertAt elem pos char = do x <- liftIO $ jsInsertAt elem pos char
                                return x


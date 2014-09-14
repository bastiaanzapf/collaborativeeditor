
{-# LANGUAGE CPP #-}

module Caret (caretPosition,characterLeftOfCaret) where

import Haste
import Haste.Prim
import Haste.App
import Haste.App.Concurrent

import Haste.Foreign

#ifdef __HASTE__

foreign import ccall jsCaretPosition :: Elem -> IO (Ptr (Maybe Int))
foreign import ccall jsCharacterLeftOfCaret :: Elem -> IO (Ptr (Maybe Int))

#else

jsCaretPosition :: Elem -> IO (Ptr (Maybe Int))
jsCaretPosition = error "jsNewHash called on server side"

jsCharacterLeftOfCaret :: Elem -> IO (Ptr (Maybe Int))
jsCharacterLeftOfCaret = error "jsNewHash called on server side"

#endif

caretPosition elem = do x <- liftIO $ jsCaretPosition elem
                        return $ fromPtr x

characterLeftOfCaret elem = do x <- liftIO $ jsCharacterLeftOfCaret elem
                               return $ fromPtr x
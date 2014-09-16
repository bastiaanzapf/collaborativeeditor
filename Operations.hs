
{-# LANGUAGE FlexibleInstances #-}

module Operations ( Operation ( Insert, Delete ) ) where

import Editor (W_Character, Id)
import Haste.Binary
import Haste.Prim
import Haste.Foreign
import WCharacter
import Haste.App

data Operation = Insert W_Character
               | Delete W_Character

    deriving ( Show )

instance Pack Operation
instance Unpack Operation

insertDummy :: Int -> Operation
insertDummy k = (Insert 
                 W_Character { WCharacter.id = Mk_Id (-20,k) , 
                               literal = 'y' ,
                               visible = True,
                               next_id = Mk_Id (-20,0),
                               previous_id = Mk_Id (-20,0) } )

instance Binary Operation where

    put (Insert w_char) = putWord8 0 >> put w_char
    put (Delete w_char) = putWord8 1 >> put w_char

    get = do cons <- getWord8
             case cons of
               0 -> do x <- get
                       return (Insert x)
               1 -> do x <- get
                       return (Delete x)

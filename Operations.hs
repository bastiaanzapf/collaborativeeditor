
{-# LANGUAGE FlexibleInstances #-}

module Operations ( Operation ( Insert, Delete ) ) where

import Editor (W_Character, Id)
import Haste.Binary
import Haste.Prim
import Haste.Foreign
import WCharacter
import Haste.App

data Operation = Insert W_Character
               | Delete Id

    deriving ( Show )

instance Pack Operation
instance Unpack Operation

insertDummy :: Int -> Operation
insertDummy k = (Insert 
                 W_Character { WCharacter.id = Mk_Id (-20,k) , 
                               literal = 'x' ,
                               visible = True,
                               next_id = Mk_Id (-20,0),
                               previous_id = Mk_Id (-20,0) } )

instance Binary Operation where
    put (Insert w_char) = putWord8 0 >> put w_char
    put (Delete w_char) = putWord8 1 >> put w_char
    get = return $ insertDummy (-30)
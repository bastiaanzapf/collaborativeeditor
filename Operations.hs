
{-# LANGUAGE FlexibleInstances #-}

module Operations ( Operation ( Insert, Delete ) ) where

import Editor (W_Character, Id)
import Haste.Binary
import Haste.Prim
import Haste.Foreign
import WCharacter

data Operation = Insert W_Character
               | Delete Id

instance Pack Operation
instance Unpack Operation

instance Binary Operation where
    put (Insert w_char) = putWord8 0 >> put w_char
    get = return (Delete $ Mk_Id (-17,0))
             
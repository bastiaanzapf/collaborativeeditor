
module Operations ( Operation ( Insert, Delete ) ) where

import Editor (W_Character, Id)
import Haste.Binary

data Operation = Insert W_Character
               | Delete Id


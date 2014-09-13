
module Operations ( Operation ( Insert, Delete ) ) where

import Editor (W_Character, Id)

data Operation = Insert W_Character
               | Delete Id
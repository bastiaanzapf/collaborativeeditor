module WCharacter ( Id (Mk_Id), W_Character ( W_Character ),
                    WCharacter.id, visible, literal, previous_id, next_id 
                  ) where

import Haste.Foreign
import Haste.Binary

data Id = Mk_Id (Int,Int) deriving (Show, Read, Eq, Ord)

instance Binary Id where
    put (Mk_Id (a,b)) = put a >> put b
    get = get

data W_Character = W_Character { id          :: WCharacter.Id
                               , visible     :: Bool
                               , literal     :: Char
                               , previous_id :: WCharacter.Id
                               , next_id     :: WCharacter.Id    } 
                   deriving (Show, Read, Eq)

instance Pack W_Character where

instance Unpack W_Character where

instance Binary W_Character where
    put w_char = putWord8 0 >> put w_char
    get = return $ W_Character {WCharacter.id=Mk_Id (-18,0),
                                visible=True,
                                literal='*',
                                next_id=Mk_Id (-19,0),
                                previous_id=Mk_Id (-20,0)
                               }
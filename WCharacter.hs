module WCharacter ( Id (Mk_Id), W_Character ( W_Character ),
                    WCharacter.id, visible, literal, previous_id, next_id 
                  ) where

import Haste.Foreign
import Haste.Binary
import Data.Char

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
    put w_char = do putWord8 0
                    putWord8 $ fromIntegral $ ord $ literal w_char
                    if (visible w_char) then putWord8 1
                                        else putWord8 0
    get = do x <- getWord8
             chr <- getWord8
             visible <- getWord8
             return $ W_Character {WCharacter.id=Mk_Id (-18,2),
                                   visible=True,
                                   literal=toEnum $ fromEnum chr,
                                   next_id=Mk_Id (-19,0),
                                   previous_id=Mk_Id (-20,0)
                                  }
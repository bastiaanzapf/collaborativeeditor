module WCharacter ( Id (Mk_Id), W_Character ( W_Character ),
                    WCharacter.id, visible, literal, previous_id, next_id 
                  ) where

import Haste.Foreign
import Haste.Binary
import Data.Char

data Id = Mk_Id (Int,Int) deriving (Show, Read, Eq, Ord)

instance Binary Id where
    put (Mk_Id (a,b)) = put a >> put b
    get = do a <- get
             b <- get
             return $ Mk_Id (a,b)

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
                    put $ WCharacter.id w_char
                    putWord32le $ fromIntegral $ ord $ literal w_char
                    if (visible w_char) then putWord8 1
                                        else putWord8 0
                    put $ previous_id w_char
                    put $ next_id w_char
    get = do x <- getWord8
             id' <- get
             lit <- getWord32le
             visible <- getWord8
             p_id <- get
             n_id <- get
             return $ W_Character {WCharacter.id=id',
                                   visible=True,
                                   literal=toEnum $ fromEnum lit,
                                   next_id=n_id,
                                   previous_id=p_id
                                  }
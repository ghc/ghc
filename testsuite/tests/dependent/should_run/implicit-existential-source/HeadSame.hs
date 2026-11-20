module HeadSame where

import Functions (Function)
import Vector

same :: Vec n a -> Vec n a -> Bool
same _ _ = True

bad :: Function a -> Function a -> a -> Bool
bad f g a = same (f a) (g a)

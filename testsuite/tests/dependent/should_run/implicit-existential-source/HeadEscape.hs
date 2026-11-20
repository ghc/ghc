module HeadEscape where

import Functions (Function)
import Vector

bad :: Function a -> a -> Vec n a
bad f a = f a

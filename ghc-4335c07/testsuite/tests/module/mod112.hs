-- !!! Hiding an abstract (Prelude) class
module M where

import Prelude hiding ( Eq )

class Eq a where
  equ :: a -> Bool

f :: Eq a => a -> Bool
f x = equ x



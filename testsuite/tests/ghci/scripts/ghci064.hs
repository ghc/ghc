{-# LANGUAGE FlexibleInstances, TypeFamilies #-}
import Data.Kind (Type)

class MyShow a where
  myshow :: a -> String

instance MyShow a => MyShow [a] where
  myshow xs = concatMap myshow xs

data T = MkT

instance MyShow T where
  myshow x = "Used generic instance"

instance MyShow [T] where
  myshow xs = "Used more specific instance"


type family F a :: Type
type instance F [a] = a -> F a
type instance F Int = Bool

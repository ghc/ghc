{-# LANGUAGE FlexibleInstances #-}
class MyShow a where
  myshow :: a -> String

instance MyShow a => MyShow [a] where
  myshow xs = concatMap myshow xs

data T = MkT

instance MyShow T where
  myshow x = "Used generic instance"

instance MyShow [T] where
  myshow xs = "Used more specific instance"

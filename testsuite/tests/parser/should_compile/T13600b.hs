module T13600b where

f !(Just x) = f !! x
f !y = head f

x = [1,2,3] ! Just 1
  where
    f !(Just x) = f !! x
    f !y = head f

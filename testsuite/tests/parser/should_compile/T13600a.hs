module T13600a where

f ! x = f !! x

x = [1,2,3] ! 1
  where
    f ! x = f !! x

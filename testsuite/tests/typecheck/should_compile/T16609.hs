module T16609 where

data T1 a = MkT1 T2
data T2 = MkT2 (T1 Maybe)

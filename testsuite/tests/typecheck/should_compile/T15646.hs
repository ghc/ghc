module T15646 where

f = 1e123456789

g x
    | 1e123456789 = 1 :: Int
    | 2e123456789 = 2

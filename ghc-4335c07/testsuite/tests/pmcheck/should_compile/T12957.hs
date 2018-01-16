module T12957 where

data A = N | A { b :: Bool }
f = case [] of (_:_) -> case () of
                          a -> undefined

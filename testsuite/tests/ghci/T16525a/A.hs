module A where

import B

myVal :: Int
myVal = sum [1,2,3,4]

value :: [Value]
value = [Value "a;lskdfa;lszkfsd;alkfjas" myVal]

v1 :: Value -> String
v1 (Value a _) = a

module A where

import B

myIntVal :: Int
myIntVal = sum [1,2,3,4]

value :: [Value]
value = [Value "a;lskdfa;lszkfsd;alkfjas" myIntVal]

v1 :: Value -> String
v1 (Value a _) = a

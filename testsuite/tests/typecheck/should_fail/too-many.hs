module TooMany where

foo :: (Int -> Int -> Bool) -> Int
foo = error "urk"

f1 :: Int -> Int -> Int -> Bool
f1 = f1

g1 = foo (f1 2 3)
     -- Here is is sensible to report
     -- f1 is applied to too many arguments

f2 :: Int -> Bool
f2 = f2

g2 = foo (f2 2)
     -- Here is is /not/ sensible to report
     -- f2 is applied to too many arguments

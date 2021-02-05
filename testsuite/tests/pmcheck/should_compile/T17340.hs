{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
module T17340 where

data A = A { a :: () }
data B = B
newtype C = C Int
pattern P = B

f_nowarn :: Bool -> Bool
f_nowarn !x = x

f :: Bool -> Bool
f True = False
f !x   = x

g :: (Int, Int) -> Bool -> ()
g (a,b) True = ()
g !x False = ()

data T = MkT !Int
h :: T -> ()
h (MkT !x) = ()

k :: Bool -> Int
k True = 1
k !_   = 2  -- clause is accessible, so warn for the bang

t :: () -> Bool -> Int
t _   True  = 1
t !() True  = 2 -- the clause has inaccessible RHS, warn for the bang
t _   False = 3

q :: Bool -> Int
q True  = 1
q !True = 2 -- clause is redundant, don't warn for the bang
q False = 3

i :: Bool -> Int
i True       = 1
i !x | x     = 2 -- redundant
     | not x = 3 -- accessible. This one will stay alive, so warn for the bang

newtype T2 a = T2 a
w :: T2 a -> Bool -> ()
w _      True = ()
w (T2 _) True = () -- redundant
w !_     True = () -- inaccessible
w _      _    = ()

z :: T2 a -> Bool -> ()
z _ True                = ()
z t2 !x | T2 _ <- t2, x = () -- redundant
        | !_ <- t2, x   = () -- inaccessible

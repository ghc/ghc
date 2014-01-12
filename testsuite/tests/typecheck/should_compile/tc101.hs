-- !!! Caused ghc-4.04proto to loop!
-- !!! (as reported by Sigbjorn)

module ShouldCompile where

-- This made the compiler (4.04 proto) loop (stack overflow)
-- The bug was in TcUnify.uUnboundVar and is documented there.

type A a = ()

f :: (A a -> a -> ()) -> ()
f = \ _ -> ()

x :: ()
x = f (\ x p -> p x)

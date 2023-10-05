{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Tcfail218 where

class C a b where foo :: (a,b)

instance                    C [Int] Bool where foo = undefined
instance                    C [a]   b   where foo = undefined
instance {-# INCOHERENT #-} C a     Int where foo = undefined


x :: ([a],Bool)
-- Needs C [a] b.
-- Should fail, as a more specific, unifying but not matching
-- non-incoherent instance exists, namely C [Int] Bool
x = foo

-- Needs C [a] Int.
-- Should succeed, because two instances match, but one is incoherent
y :: ([a],Int)
y = foo


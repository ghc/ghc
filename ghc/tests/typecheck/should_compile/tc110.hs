{-# OPTIONS -fglasgow-exts #-}

module ShouldCompile where

-- A stripped down functional-dependency 
-- example that causes GHC 4.08.1 to crash with:
-- "basicTypes/Var.lhs:194: Non-exhaustive patterns in function readMutTyVar"
-- Reported by Thomas Hallgren Nov 00


primDup :: Int -> IO Int
primDup = undefined

dup () = call primDup

class Call    c h | c -> h where
    call  :: c -> h

instance Call c h => Call (Int->c) (Int->h) where 
    call f = call . f



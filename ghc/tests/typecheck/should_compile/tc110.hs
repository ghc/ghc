{-# OPTIONS -fglasgow-exts #-}

module ShouldCompile where

-- A stripped down functional-dependency 
-- example that causes GHC 4.08.1 to crash with:
-- "basicTypes/Var.lhs:194: Non-exhaustive patterns in function readMutTyVar"
-- Reported by Thomas Hallgren Nov 00


primDup :: Int -> IO Int
primDup = undefined

dup () = call primDup

-- 	call :: Call c h => c -> h
--
--	call primDup :: h  with  {Call (Int -> IO Int) h}
--	Hence h must be fixed by the environment
--	Reduce at top level to {Call (IO Int) h'}

class Call    c h | c -> h where
    call  :: c -> h

instance Call c h => Call (Int->c) (Int->h) where 
    call f = call . f



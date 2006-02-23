{-# OPTIONS -fglasgow-exts -fallow-undecidable-instances #-}
-- -fallow-undecidable-instances now needed because the Coverage Condition fails

module ShouldFail where

-- A stripped down functional-dependency 
-- example that causes GHC 4.08.1 to crash with:
-- "basicTypes/Var.lhs:194: Non-exhaustive patterns in function readMutTyVar"
-- Reported by Thomas Hallgren Nov 00


primDup :: Int -> IO Int
primDup = undefined

dup () = call primDup

-- 	call :: Call c h => c -> h
--
--	call primDup :: {Call (Int -> IO Int) h} => h  with  
--  Using the instance decl gives
--	call primDup :: {Call (IO Int) h'} => Int -> h'
--  The functional dependency means that h must be constant
--  Hence program is rejected because it can't find an instance 
--  for {Call (IO Int) h'}

class Call    c h | c -> h where
    call  :: c -> h

instance Call c h => Call (Int->c) (Int->h) where 
    call f = call . f



{-# OPTIONS_GHC -fglasgow-exts -O -ddump-prep #-}
module Max(result) where

foo ys = foldr (\x xs -> x : reverse xs) [] ys

result xs = 
    let stuff = [1, 1, 1, 1, 1, 1]
    in foo (reverse stuff)

-- What used to happen is that foldr got expanded by main simplification
-- and the resulting "go" function got floated out but because we manufactured 
-- a new binder for it in newPolyBndrs we would lose its demand signature! 
-- This means that the later application of it in result did not use call by value :-(

-- Eyeball test:
--  Ensure that Max.poly_go has a demand signature
--  Ensure that we use call by value to call Max.poly_go in result 
--	i.e. the call to Max.poly_go inside Max.result looks like this:
--
-- case GHC.List.poly_rev @ t1_a6x sat_seb (GHC.Base.[] @ t1_a6x)
--        of sat_sed { __DEFAULT ->
--        Max.poly_go @ t1_a6x sat_sed
--        } } in

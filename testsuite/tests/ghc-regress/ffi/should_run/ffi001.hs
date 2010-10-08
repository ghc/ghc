

-- !!! A simple FFI test

-- This one provoked a bogus renamer error in 4.08.1:
--	panic: tcLookupGlobalValue: <THIS>.PrelIOBase.returnIO{-0B,s-}
-- (the error was actually in DsMonad.dsLookupGlobalValue!)

module Main where

import Foreign

foreign export ccall "gccd" mygcd :: Int -> Int -> Int 

main = putStrLn "No bug"

mygcd a b = if (a==b) then a 
	    else if (a<b) then mygcd a (b-a)
	    else mygcd (a-b) a

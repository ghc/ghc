-- This module should run fine with an empty argument list
-- But it won't if the strictness analyser thinks that 'len' is use 
-- strictly, which was the case in GHC 6.0

-- See the io_hack_reqd in DmdAnal.lhs

module Main where

import System.Environment
import System.Exit

main    = do
            args <- getArgs
            let len = read (head args) :: Int

            (if null args && useLazily len
	        then putStrLn "ok" >> exitWith ExitSuccess 
		else return () ) 

            print len

useLazily :: Int -> Bool
useLazily len = ([len,3,4] !! 1) == 3

module Main where

import Prelude hiding (catch)
import Exception
import IO hiding (try, catch)

main = do
  ioTest
  errorTest
  noMethodTest
  patMatchTest
  guardTest
  dynTest

ioTest :: IO ()
ioTest = catchIO (fail (userError "wibble")) 
	   (\ex -> if isUserError ex then putStr "io exception caught\n" 
				     else error "help!")

errorTest :: IO ()
errorTest = getException (1 + error "call to 'error'") >>= \r ->
	    case r of
		Just exception -> putStr "error call caught\n"
		Nothing        -> error "help!"

instance (Show a, Eq a) => Num (Maybe a) where {}

noMethodTest :: IO ()
noMethodTest = getException (Just () + Just ()) >>= \ r ->
	case r of
		Just (NoMethodError err) -> putStr "no method error\n"
		other                    -> error "help!"

patMatchTest :: IO ()
patMatchTest = catch (case test1 [1..10] of () -> return ())
  (\ex -> case ex of
		PatternMatchFail err -> putStr err
		other 		     -> error "help!")
		  
test1 [] = ()

guardTest = catch (case test2 of () -> return ())
  (\ex -> case ex of
		NonExhaustiveGuards err -> putStr err
		other 		     -> error "help!")

test2 | all (==0) [1] = ()

dynTest = catchDyn (case throwDyn (42::Int, (+1)::Int->Int) of () -> return ())
  (\(i,f) -> let x = f (i::Int) :: Int in putStr (show x))

{-
recSelTest
recConTest
recUpdTest
assertTest
arithTest
-}

module Main where

import Prelude hiding (catch)
import Control.OldException 

main = do
  ioTest
  errorTest
  noMethodTest
  patMatchTest
  guardTest
  dynTest

ioTest :: IO ()
ioTest = catchJust userErrors (ioError (userError "wibble")) 
	   (\ex -> putStr "user exception caught\n")

errorTest :: IO ()
errorTest = try (evaluate (1 + error "call to 'error'")) >>= \r ->
	    case r of
		Left exception -> putStr "error call caught\n"
		Right _        -> error "help!"

instance (Show a, Eq a) => Num (Maybe a) where {}

noMethodTest :: IO ()
noMethodTest = try (evaluate (Just () + Just ())) >>= \ r ->
	case r of
		Left (NoMethodError err) -> putStr "no method error\n"
		Right _                  -> error "help!"

patMatchTest :: IO ()
patMatchTest = catch (case test1 [1..10] of () -> return ())
  (\ex -> case ex of
		PatternMatchFail err -> putStr err
		other 		     -> error "help!")
		  
test1 [] = ()

guardTest = catch (case test2 of () -> return ())
  (\ex -> case ex of
		PatternMatchFail err -> putStr err
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

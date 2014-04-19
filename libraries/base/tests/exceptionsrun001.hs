module Main where

import Control.Exception
import System.IO.Error

main = do
  ioTest
  errorTest
  noMethodTest
  patMatchTest
  guardTest

ioTest :: IO ()
ioTest = catchJust (\e -> if isUserError e then Just () else Nothing)
                   (ioError (userError "wibble"))
                   (\() -> putStrLn "user exception caught")

errorTest :: IO ()
errorTest = do r <- try (evaluate (1 + error "call to 'error'"))
               case r of
                   Left (ErrorCall _) -> putStrLn "error call caught"
                   Right _            -> error "help!"

instance (Show a, Eq a) => Num (Maybe a) where {}

noMethodTest :: IO ()
noMethodTest = do r <- try (evaluate (Just () + Just ()))
                  case r of
                      Left (NoMethodError err) -> putStrLn "no method error"
                      Right _                  -> error "help!"

patMatchTest :: IO ()
patMatchTest = catch (case test1 [1..10] of () -> return ())
  (\ex -> case ex of
          PatternMatchFail err -> putStr err
          _                    -> error "help!")

test1 [] = ()

guardTest = catch (case test2 of () -> return ())
                  (\ex -> case ex of
                          PatternMatchFail err -> putStr err
                          _                    -> error "help!")

test2 | all (==0) [1] = ()


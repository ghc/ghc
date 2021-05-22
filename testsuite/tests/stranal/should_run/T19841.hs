module Main where

import Control.Exception
import Debug.Trace
import System.IO

data MyException = MyException deriving Show
instance Exception MyException

main :: IO ()
main = handle (\ MyException -> hPutStrLn stderr "Exception") $ do
  trace "Debugging" 5
  `seq` throw MyException
  `seq` hPutStrLn stderr "Survived Exception"

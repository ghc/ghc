-- test performance of a mutable store represented as an Array of IORefs.

module Main where
import Store1
--import Store2
import Control.Monad
import System.Environment

main = 
  do [n] <- fmap (fmap read) getArgs
     ss <- replicateM n mkStore
     replicateM_ 5 (mapM_ testSequence ss)

testSequence :: Store -> IO ()
testSequence s = 
  do replicateM_ 5 (addElemToBucket s 3 17)

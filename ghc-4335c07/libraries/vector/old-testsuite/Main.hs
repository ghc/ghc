module Main where

import Testsuite.Stream as Stream
import Testsuite.Vector as Vector
import Testsuite.Utils.Test

main =
  do
    (_, s) <- execTestM tests
    mapM_ putStrLn (summarise s)
  where
    tests = mapM_ runTest [Stream.tests, Vector.tests]


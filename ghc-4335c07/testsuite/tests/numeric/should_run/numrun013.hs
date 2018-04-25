
-- Test for trac #1042

import Control.Exception
import Data.Int

main :: IO ()
main = do print ((minBound :: Int) `div` (-1))   `myCatch` print
          print ((minBound :: Int8) `div` (-1))  `myCatch` print
          print ((minBound :: Int16) `div` (-1)) `myCatch` print
          print ((minBound :: Int32) `div` (-1)) `myCatch` print
          print ((minBound :: Int64) `div` (-1)) `myCatch` print

myCatch :: IO a -> (ArithException -> IO a) -> IO a
myCatch = catch


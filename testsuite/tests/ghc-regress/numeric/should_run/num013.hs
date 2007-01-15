
-- Test for trac #1042

import Control.Exception
import Data.Int
import Data.Word
import Prelude hiding (catch)

main :: IO ()
main = do print ((minBound :: Int) `div` (-1)) `catch` print
          print ((minBound :: Int8) `div` (-1)) `catch` print
          print ((minBound :: Int16) `div` (-1)) `catch` print
          print ((minBound :: Int32) `div` (-1)) `catch` print
          print ((minBound :: Int64) `div` (-1)) `catch` print


module Main where
import Data.Array.IO
import Control.Monad
main = do
  arrs <- sequence $ repeat $ (newArray_ (0,2^28) :: IO (IOUArray Int Int))
        -- larger than 2^28 causes other problems...
  print (length arrs)

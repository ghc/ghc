-- Test for bug #1791
import Control.Monad.ST 
import Data.Array.ST
import Data.Array.MArray
import Data.Array.Base(unsafeNewArray_)
main = print (runST (do make_empty_table >> return ()))

make_empty_table::  ST s (STArray s (Int, Int) (Maybe ep))
make_empty_table = 
       unsafeNewArray_ ((1, 1), (16384, 16384))

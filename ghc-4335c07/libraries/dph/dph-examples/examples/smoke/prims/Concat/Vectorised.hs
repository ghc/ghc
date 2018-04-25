
{-# LANGUAGE ParallelArrays #-}
{-# OPTIONS -fvectorise #-}
module Vectorised 
        (test0, test1, test2, test3, test4)
where
import Data.Array.Parallel
import Data.Array.Parallel.Prelude.Int  as I
import qualified Prelude as P

arr2   :: [:[:Int:]:]
arr2    = [: [:0, 1, 2, 3:], [:4, 5:], [::], [:6, 7, 8:], [:9:]:]

arr3   :: [:[:[:Int:]:]:]
arr3    = [: [:[:0:]:], [:[:1:], [:2, 3:]:], [:[:4, 5:]:], [::], [:[:6, 7:], [:8:]:], [:[::]:], [:[:9:]:] :]


test0'  :: [:Int:]
test0'  = concatP arr2
test0   = toPArrayP test0'

test1'  :: [:Int:]
test1'  = concatP (concatP arr3)
test1   = toPArrayP test1'

test2'  :: [:Int:]
test2'  = concatP (mapP concatP arr3)
test2   = toPArrayP test2'


test3'  :: [:Int:]
test3'  = concatP [: mapP (I.+ 1) x | x <- [: [:1, 2:], [:3, 4:] :] :]
test3   = toPArrayP test3'


test4'   :: [:Int:]
test4'   = concatP (mapP (mapP (I.+ 1)) arr2)
test4    = toPArrayP test4'

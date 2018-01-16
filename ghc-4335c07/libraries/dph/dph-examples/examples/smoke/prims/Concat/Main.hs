
-- | Test out contatentation. 
--   Concatenation is a core operation that exercises the representation of nested
--   arrays, as well as the extractsPR function from the dph-lifted-vseg library.
import Vectorised 
import Test.HUnit
import qualified Data.Array.Parallel.PArray     as PA

arr2   :: [[Int]]
arr2    = [ [0, 1, 2, 3], [4, 5], [], [6, 7, 8], [9]]

arr3   :: [[[Int]]]
arr3    = [[[0]], [[1], [2, 3]], [[4, 5]], [], [[6, 7], [8]], [[]], [[9]] ]


main    = runTestTT $ test $ 
        [ "test0" ~: PA.toList test0    ~?= concat arr2
        , "test1" ~: PA.toList test1    ~?= concat (concat arr3) 
        , "test2" ~: PA.toList test2    ~?= concat (map concat arr3)
        , "test3" ~: PA.toList test3    ~?= concat [ map (+1) x | x <- [ [1, 2], [3, 4] ] ]
        , "test4" ~: PA.toList test4    ~?= concat (map (map (+1)) arr2)
        ]


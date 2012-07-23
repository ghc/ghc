module Main(main) where

import Memo1

testMemo = do
   let keys = [ [1..n] | n <- [1..1000] ]
       keys2 = [ [n,n-1..1] | n <- [1..1000] ]
       mlength = memo length
   putStr (show (map mlength (keys ++ keys ++ keys2 ++ keys2)))
   putStr (show (mlength [1..100000]))

-- mlength will memoize itself over each element of 'keys', returning
-- the memoized result the second time around.  Then we move onto
-- keys2, and while we're doing this the first lot of memo table
-- entries can be purged.  Finally, we do a a large computation
-- (length [1..10000]) to allow time for the memo table to be fully
-- purged.

main = testMemo

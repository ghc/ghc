module T15907 where

import T15907A

testMemo :: IO ()
testMemo = do
  let keys = [[1 .. n] | n <- [1 .. 1000]]
      keys2 = [[n, n - 1 .. 1] | n <- [1 .. 1000]]
      mlength = memo length
  putStr (show (map mlength (keys ++ keys ++ keys2 ++ keys2)))
  putStr (show (mlength [1 .. 100000]))

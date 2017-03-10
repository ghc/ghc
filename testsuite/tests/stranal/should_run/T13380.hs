import Control.Exception

-- This should result in the "What" exception, not the undefined.
{-# NOINLINE f #-}
f :: Int -> Int -> IO Int
f x y | x>0       = throwIO (userError "What")
      | y>0       = return 1
      | otherwise = return 2

main = f 2 undefined >>= print

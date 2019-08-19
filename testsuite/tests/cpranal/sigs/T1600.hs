-- | Basically tests Nested CPR on IO.
module T1600 where

facIO :: Int -> IO Int
facIO n | n < 2     = return 1
        | otherwise = do n' <- facIO (n-1); return (n*n')

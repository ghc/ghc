module Lib where


-- pretty strict
fac1 :: Int -> a ->  (a, Int)
fac1 n s | n < 2     = (s,1)
         | otherwise = case  fac1 (n-1) s of (s',n') -> let n'' = n*n' in n'' `seq` (s',n'')

-- lazier, but Int still has CPR
fac2 :: Int -> a ->  (a, Int)
fac2 n s | n < 2     = (s,1)
         | otherwise = case  fac2 (n-1) s of (s',n') -> (s',n'*n')

-- even lazier, but evaluation of the Int doesn't terminate rapidly!
-- Thus, we may not WW for the nested Int.
-- Otherwise @fac3 99999 () `seq` ()@ (which should terminate rapidly)
-- evaluates more than necessary.
fac3 :: Int -> a ->  (a, Int)
fac3 n s | n < 2     = (s,1)
         | otherwise = let (s',n') = fac3 (n-1) s in (s',n'*n')

facIO :: Int -> IO Int
facIO n | n < 2     = return 1
        | otherwise = do n' <- facIO (n-1); return (n*n')

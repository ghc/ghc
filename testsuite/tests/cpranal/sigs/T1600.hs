module T1600 where


-- pretty strict
fac :: Int -> a ->  (a, Int)
fac n s | n < 2     = (s,1)
        | otherwise = case  fac (n-1) s of (s',n') -> let n'' = n*n' in n'' `seq` (s',n'')

-- lazier, but Int still has CPR
fac2 :: Int -> a ->  (a, Int)
fac2 n s | n < 2     = (s,1)
         | otherwise = case  fac (n-1) s of (s',n') -> (s',n'*n')

-- even lazier, no CPR on Int
fac3 :: Int -> a ->  (a, Int)
fac3 n s | n < 2     = (s,1)
         | otherwise = let (s',n') = fac (n-1) s in (s',n'*n')

facIO :: Int -> IO Int
facIO n | n < 2     = return 1
        | otherwise = do n' <- facIO (n-1); return (n*n')

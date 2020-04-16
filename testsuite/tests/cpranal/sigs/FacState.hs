module FacState where


fac :: Int -> a ->  (a, Int)
fac n s | n < 2     = (s,1)
        | otherwise = case  fac (n-1) s of (s',n') -> let n'' = n*n' in n'' `seq` (s',n'')

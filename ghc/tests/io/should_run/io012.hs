import IO -- 1.3

import CPUTime

main = do
    h <- openFile "/dev/null" WriteMode
    hPrint h (nfib 30)
    t <- getCPUTime
    print (length (show t)) -- printing the CPU time itself is un-cool if you want to diff the output..

nfib :: Integer -> Integer
nfib n 
  | n <= 1 = 1
  | otherwise = (n1 + n2 + 1)
  where 
    n1 = nfib (n-1) 
    n2 = nfib (n-2)

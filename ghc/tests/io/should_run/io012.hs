import IO -- 1.3

import CPUTime

main = 
    openFile "/dev/null" WriteMode >>= \ h ->
    hPrint h (nfib 30) >>
    getCPUTime >>= \ t ->
    print t

nfib :: Integer -> Integer
nfib n 
  | n <= 1 = 1
  | otherwise = (n1 + n2 + 1)
  where 
    n1 = nfib (n-1) 
    n2 = nfib (n-2)

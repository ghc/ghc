import LibCPUTime

main = 
    openFile "/dev/null" WriteMode >>= \ h ->
    hPutText h (nfib 30) >>
    getCPUTime >>= \ t ->
    putText t >>
    putChar '\n'

nfib :: Integer -> Integer
nfib n 
  | n <= 1 = 1
  | otherwise = (n1 + n2 + 1)
  where 
    n1 = nfib (n-1) 
    n2 = nfib (n-2)

-- !!! Test getCPUTime

import System.CPUTime
import System.IO

main :: IO ()
main = do
    t28 <- timeFib 28
    t29 <- timeFib 29
    t30 <- timeFib 30
    print (t28 <= t29, t29 <= t30)

timeFib :: Integer -> IO Integer
timeFib n = do
    start <- getCPUTime
    print (nfib n)
    end <- getCPUTime
    return (end - start)

nfib :: Integer -> Integer
nfib n
  | n <= 1 = 1
  | otherwise = (n1 + n2 + 1)
  where
    n1 = nfib (n-1)
    n2 = nfib (n-2)

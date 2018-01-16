-- The Computer Language Benchmarks Game
-- http://benchmarksgame.alioth.debian.org/
-- contributed by Bryan O'Sullivan
-- modified by Eugene Kirpichov: pidgits only generates
-- the result string instead of printing it. For some
-- reason, this gives a speedup.

import System.Environment

pidgits n = 0 % (0 # (1,0,1)) where
 i%ds
  | i >= n = []
  | True = (concat h ++ "\t:" ++ show j ++ "\n") ++ j%t
  where k = i+10; j = min n k
        (h,t) | k > n = (take (n`mod`10) ds ++ replicate (k-n) " ",[])
              | True = splitAt 10 ds
 j # s | n>a || r+n>=d = k # t
     | True = show q : k # (n*10,(a-(q*d))*10,d)
  where k = j+1; t@(n,a,d)=k&s; (q,r)=(n*3+a)`divMod`d
 j&(n,a,d) = (n*j,(a+n*2)*y,d*y) where y=(j*2+1)

main = putStr.pidgits.read.head =<< getArgs

-- Caused a crash in GHC 6.4 when optimising, due to inlining of runST too early.

-- Spectral Norm benchmark

import Data.Array
import System.Environment (getArgs)

main = do
        --[arg] <- getArgs
        --let n = (read arg) - 1 
        let n = 80
        let init = listArray (0,n) (repeat 1.0)
        let (v:u:rest) = drop 19 $ iterate (eval_AtA_times_u n) init
        let vBv = sum [(u!i)*(v!i) |i<-[0..n]]
        let vv  = sum [(v!i)*(v!i) |i<-[0..n]]
        print $ sqrt (vBv/vv)

eval_AtA_times_u n u = eval_At_times_u n v
    where v = eval_A_times_u n u

eval_A x y = 1.0/((i+j)*(i+j+1)/2+i+1)
    where i = fromIntegral x
          j = fromIntegral y

eval_A_times_u n u = accumArray (+) 0 (0,n)
                     [(i,(eval_A i j) * u!j)|i<-[0..n], j<-[0..n]]

eval_At_times_u n u = accumArray (+) 0 (0,n)
                      [(i,(eval_A j i) * u!j)|i<-[0..n], j<-[0..n]]
 

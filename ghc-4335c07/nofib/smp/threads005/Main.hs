{-# OPTIONS_GHC -O2 #-}

-- Program from GHC ticket #1589, to test scaling of the RTS with many threads.

{-
On a 1.86GHz Intel Xeon:

GHC 6.8.3, with -threaded:

$ ./1589 100000 1 
Creating pipeline with 100000 processes in it.
Pumping a single message through the pipeline.
Pumping a 1 messages through the pipeline.
       n   create    pump1    pump2 create/n  pump1/n  pump2/n
                s        s        s       us       us       us
  100000    5.951    0.520    0.462    59.51     5.20     4.62

GHC 6.10.1, without -threaded (NB. 10 times more threads):

$ ./1589 1000000 1
Creating pipeline with 1000000 processes in it.
Pumping a single message through the pipeline.
Pumping a 1 messages through the pipeline.
       n   create    pump1    pump2 create/n  pump1/n  pump2/n
                s        s        s       us       us       us
 1000000    4.029    0.915    0.911     4.03     0.91     0.91

GHC 6.10.1, with -threaded:

$ ./1589 1000000 1                         
Creating pipeline with 1000000 processes in it.
Pumping a single message through the pipeline.
Pumping a 1 messages through the pipeline.
       n   create    pump1    pump2 create/n  pump1/n  pump2/n
                s        s        s       us       us       us
 1000000    4.255    1.071    1.070     4.26     1.07     1.07


In fact these numbers bounce around quite a bit, they aren't accurate
to more than 20\% or so.
-}

import System.IO
import System.Environment
import System.CPUTime
import Text.Printf
import Control.Monad
import Control.Concurrent
import Control.Concurrent.MVar

type Msg = (Int, String)

nthreadsDefault :: Int
nthreadsDefault = 10000

npumpDefault :: Int
npumpDefault = 100

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    args <- getArgs
    let (nthreads, npump) =
            case args of
            [] -> (nthreadsDefault, npumpDefault)
            [arg] -> (read arg, npumpDefault)
            [arg1,arg2] -> (read arg1, read arg2)
            _ -> error "Use 0, 1, or 2 arguments\n"
    printf "Creating pipeline with %d processes in it.\n" nthreads
    t1s <- getCPUTimeDouble
    s <- newEmptyMVar
    e <- createMany nthreads s
    t1e <- getCPUTimeDouble
    printf "Pumping a single message through the pipeline.\n"
    t2s <- getCPUTimeDouble
    pump 1 s e "Hello, World!"
    t2e <- getCPUTimeDouble
    printf "Pumping a %d messages through the pipeline.\n" npump
    t3s <- getCPUTimeDouble
    pump npump s e "x"
    t3e <- getCPUTimeDouble
    let ct = t1e - t1s
        p1 = t2e - t2s
        p2 = t3e - t3s
        n = fromIntegral nthreads * 1e-6
        p = fromIntegral npump
    printf "       n   create    pump1    pump2 create/n  pump1/n  pump2/n\n"
    printf "                s        s        s       us       us       us\n"
    printf "%8d %8.3f %8.3f %8.3f %8.2f %8.2f %8.2f\n" nthreads ct p1 p2 (ct/n) (p1/n) (p2/n/p)

pump :: Int -> MVar Msg -> MVar Msg -> String -> IO ()
pump n s e t = do
    forkIO $ replicateM_ n $ putMVar s (0, t)
    replicateM_ n $ do
        msg <- takeMVar e
        when (t /= snd msg) $
            error "Distorted message"

createMany :: Int -> MVar Msg -> IO (MVar Msg)
createMany 0 v = return v
createMany n v = do
    o <- newEmptyMVar
    forkIO $ copy v o
    createMany (n-1) o

copy :: MVar Msg -> MVar Msg -> IO ()
copy i o = do
    (n, v) <- takeMVar i
    let n' = n+1
    seq n' (putMVar o (n', v))
    copy i o

getCPUTimeDouble :: IO Double
getCPUTimeDouble = do
    t <- getCPUTime
    return $ fromInteger t * 1e-12

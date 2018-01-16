-- Written by Bertram Felgenhauer
--
-- https://ghc.haskell.org/trac/ghc/ticket/11760#comment:14
--
-- Compile with -threaded -with-rtsopts=-N2

{-# LANGUAGE BangPatterns #-}
import Control.Concurrent
import Control.Monad
import Control.Monad.ST.Lazy
import Control.Exception
import Data.STRef
import Data.IORef
import Control.Concurrent.MVar
import Data.List

-- evil ST action that tries to synchronize (by busy waiting on the
-- shared STRef) with a concurrent evaluation
evil :: ST s [Int]
evil = do
    r <- strictToLazyST $ newSTRef 0
    replicateM 100 $ do
        i <- strictToLazyST $ readSTRef r
        let !j = i + 1
        strictToLazyST $ writeSTRef r j
        let go 0 = return ()
            go n = do
                i' <- strictToLazyST $ readSTRef r
                when (j == i') $ go (n-1)
        go 100
        return j

main = do
    let res = runST evil
    s0 <- newIORef (map pred (0 : res))
    s1 <- newIORef (map pred (1 : res))
    m0 <- newMVar ()
    m1 <- newMVar ()
    forkIO $ do
        putMVar m0 ()
        readIORef s0 >>= evaluate . foldl' (+) 0
        putMVar m0 ()
    forkIO $ do
        putMVar m1 ()
        readIORef s1 >>= evaluate . foldl' (+) 0
        putMVar m1 ()
    threadDelay 10000
    replicateM 3 $ takeMVar m0 >> takeMVar m1
    v0 <- tail <$> readIORef s0
    v1 <- tail <$> readIORef s1
    print (v0 == v1)

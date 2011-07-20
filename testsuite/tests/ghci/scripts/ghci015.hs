-- Code from ticket #488

module Test where 
 
import Control.Concurrent.STM 
import Control.Concurrent 
import Control.Exception 
import Prelude hiding (catch) 
 
 
runTest loop = do 
    (tc1, tc2, tmv) <- atomically (do 
        tmv <- newEmptyTMVar 
        tc1 <- newTChan 
        tc2 <- newTChan 
        return (tc1, tc2, tmv) 
        ) 
    myTId <- myThreadId 
    forkIO (forked loop (tc1, tc2, tmv, myTId)) 
    atomically (writeTChan tc1 "blah") 
    atomically (writeTChan tc1 "blah2") 
    return "done" 
 
 
forked loop args@(tc1, tc2, tmv, hisTId) = catch ((loop args) >>= setTMV . Just) hndlr `finally` setTMV Nothing 
        where 
            setTMV x = atomically (tryPutTMVar tmv x >> return ()) 
            hndlr (AsyncException ThreadKilled) = return () 
            hndlr e                             = throwTo hisTId e 
 
goodLoop args@(tc1, tc2, tmv, hisTId) = do 
    x <- atomically (readTChan tc1) 
    x' <- return $ reverse x 
    atomically (writeTChan tc2 x') 
    if x == "blah2" 
        then return () 
        else goodLoop args 
 
badLoop args@(tc1, tc2, tmv, hisTId) = do 
    x <- atomically (readTChan tc1) 
    x' <- return $ reverse x 
    atomically (writeTChan tc2 x') 
    badLoop args

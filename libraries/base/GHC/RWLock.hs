{-# LANGUAGE NoImplicitPrelude #-}

module GHC.RWLock ( RWLock, newRWLock, withReader, withWriter ) where

import GHC.Base
import GHC.Conc.TVar
import GHC.Exception
import GHC.Num ((+), (-))

data RWState = Free
             | Readers !Int
             | Writer

-- | Read-biased readers-writer lock.
data RWLock a = RWLock { lockState :: !(TVar RWState)
                       , lockValue :: !(TVar a) }

newRWLock :: a -> IO (RWLock a)
newRWLock x = do
    state <- newTVarIO Free
    value <- newTVarIO x
    return $ RWLock state value

withReader :: RWLock a -> (a -> IO b) -> IO b
withReader lock k = do
    x <- atomically begin
    r <- k x
    atomically end
    return r
  where
    begin = do
        s <- readTVar (lockState lock)
        s' <- case s of
              Free      -> return $ Readers 1
              Readers n -> return $ Readers (n+1)
              Writer    -> retry
        writeTVar (lockState lock) s'
        readTVar (lockValue lock)

    end = do
        s <- readTVar (lockState lock)
        s' <- case s of
              Readers 1 -> return Free
              Readers n -> return $ Readers (n-1)
              _         -> error "this can't happen"
        writeTVar (lockState lock) s'

withWriter :: RWLock a -> (a -> IO (a,b)) -> IO b
withWriter lock k = do
    x <- atomically begin
    (x', r) <- k x
    atomically (end x')
    return r
  where
    begin = do
        s <- readTVar (lockState lock)
        s' <- case s of
              Free      -> return Writer
              Readers n -> retry
              Writer    -> retry
        writeTVar (lockState lock) s'
        readTVar (lockValue lock)

    end x' = do
        s <- readTVar (lockState lock)
        s' <- case s of
              Writer -> return Free
              _      -> error "this can't happen"
        writeTVar (lockState lock) s'
        writeTVar (lockValue lock) x'

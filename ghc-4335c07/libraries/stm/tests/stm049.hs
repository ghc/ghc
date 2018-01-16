-- STM stress test

{-# LANGUAGE ForeignFunctionInterface #-}
module Main (main) where

import Control.Concurrent
import Control.Concurrent.STM
import System.Random
import Data.Array
import GHC.Conc         ( unsafeIOToSTM )
import Control.Monad    ( when )
import System.IO
import System.IO.Unsafe
import System.Environment
import Foreign.C

-- | The number of bank accounts
n_accounts :: Int
n_accounts = 7

-- | The number of threads transferring money between accounts
n_actors :: Int
n_actors = 10

-- | The max initial number of monetary units in each account
init_credit :: Int
init_credit = 5

-- | The maximum size of a transfer
max_transfer :: Int
max_transfer = 3

-- | The maximum amount transferred by the source/sink thread
max_source :: Int
max_source = 3

max_transactions = 2000 :: Int

type Accounts = Array Int (TVar Int)

thread :: Int -> TVar Int -> Accounts -> IO ()
thread tid done accounts = loop max_transactions
 where loop 0 = atomically $ do x <- readTVar done; writeTVar done (x+1)
       loop n = do
          src    <- randomRIO (1,n_accounts)
          dst    <- randomRIO (1,n_accounts)
          if (src == dst) then loop n else do
          amount <- randomRIO (1,max_transfer)
          start tid src dst amount
          atomically_ tid  $ do
            let src_acc = accounts ! src
                dst_acc = accounts ! dst
            credit_src <- readTVar src_acc
            when (credit_src < amount) retry
            writeTVar src_acc (credit_src - amount)
            credit_dst <- readTVar dst_acc
            writeTVar dst_acc (credit_dst + amount)
          loop (n-1)

start tid src dst amount =
  puts ("start " ++ show tid ++ ' ':show src ++ ' ':show dst ++ ' ':show amount)

main = do
  hSetBuffering stdout LineBuffering

{-
  args <- getArgs
  case args of
   [n,m] -> let g = read (n ++ ' ':m) in setStdGen g
   []    -> do g <- getStdGen
               print g
-}

  -- for a deterministic run, we set the random seed explicitly:
  setStdGen (read "526454551 6356")

  -- HACK: the global commitVar requires atomically, so we want to seq it outside of
  -- an enclosing atomically (otherwise STM gets very confused).
  seq commitVar $ return ()

--  print n_actors
--  print n_accounts
  amounts <- sequence (take n_accounts (repeat (randomRIO (0,init_credit))))
--  mapM print amounts
  tvars <- atomically $ mapM newTVar amounts
  let accounts = listArray (1,n_accounts) tvars
  done <- atomically (newTVar 0)
  sequence [ forkIO (thread id done accounts) | id <- [1..n_actors] ]
  forkIO $ sourceSinkThread accounts
  atomically $ do
    x <- readTVar done
    when (x < n_actors) retry

sourceThreadId = 0 :: Int
sourceAccount  = 0 :: Int

-- A thread that alternates between dropping some cash into an account
-- (source), and removing some cash from an account (sink).
sourceSinkThread accounts = loop True
  where loop source = do
           amount <- randomRIO (1,max_source)
           acct   <- randomRIO (1,n_accounts)
           if source
                then do start sourceThreadId sourceAccount acct amount
                        transfer acct amount
                else do start sourceThreadId acct sourceAccount amount
                        transfer acct (-amount)
           loop (not source)

        transfer acct amount = do
          let t = accounts ! acct
          atomically_ sourceThreadId $ do
            x <- readTVar t
            writeTVar t $! max 0 (x+amount) -- never drop below zero,
                                           -- and don't block.

    -- NB. $! above is necessary to avoid this test getting into a bad
    -- state.  The sourceSinkThread fills up all the accounts with
    -- thunks which the other threads have to evaluate.  They'll keep
    -- getting blocked on each other, and meanwhile the
    -- sourceSinkThread can keep on filling up the accounts with more
    -- thunks.

-- -----------------------------------------------------------------------------
-- Our tracing wrapper for atomically

{-# NOINLINE commitVar #-}
commitVar = unsafePerformIO $ atomically $ newTVar ([] :: [Int])

atomically_ :: Int -> STM a -> IO a
atomically_ tid stm = do
  r <- atomically $ do
    stmTrace ("execute " ++ show tid)
    r <- stm `orElse` do
                stmTrace ("retry " ++ show tid)
                retry
    c <- readTVar commitVar
    writeTVar commitVar (tid:c)
    return r

  atomically $ do
    c <- readTVar commitVar
    mapM stmTrace ["commit " ++ show tid | tid <- reverse c ]
    writeTVar commitVar []
  return r

stmTrace s = unsafeIOToSTM (puts s)

puts :: String -> IO ()
puts s = throwErrnoIfMinus1_ "puts" $ withCString s c_puts

foreign import ccall unsafe {-"puts"-} "strlen"
  c_puts :: CString -> IO CInt

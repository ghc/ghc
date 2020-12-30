{-# LANGUAGE CPP #-}

-- Benchmark 'threadDelay' by forking N threads which sleep for a
-- number of milliseconds and wait for them all to finish.

import Args (ljust, parseArgs, positive, theLast)
import Control.Concurrent (forkIO, runInUnboundThread)
import Control.Monad (when)
import Data.Function (on)
import Data.Monoid (Monoid(..), Last(..))
import System.Console.GetOpt (ArgDescr(ReqArg), OptDescr(..))
import System.Environment (getArgs)
import System.Event.Thread (ensureIOManagerIsRunning)
import Control.Concurrent.STM

#ifdef USE_GHC_IO_MANAGER
import Control.Concurrent (threadDelay)
#else
import System.Event.Thread (threadDelay)
#endif

main = do
    (cfg, _) <- parseArgs defaultConfig defaultOptions =<< getArgs
    let numThreads = theLast cfgNumThreads cfg

    ensureIOManagerIsRunning
    done <- newTVarIO False
    ref <- newTVarIO 0
    let loop :: Int -> IO ()
        loop i = do
            when (i < numThreads) $ do
                _ <- forkIO $ do
                   threadDelay 1000
                   atomically $ do
                     a <- readTVar ref
                     let !b = a+1
                     writeTVar ref b
                     when (b == numThreads) $ writeTVar done True
                loop (i + 1)
    runInUnboundThread $ do
      loop 0
      atomically $ do
        b <- readTVar done
        when (not b) retry

------------------------------------------------------------------------
-- Configuration

data Config = Config {
      cfgNumThreads :: Last Int
    }

defaultConfig :: Config
defaultConfig = Config
    { cfgNumThreads    = ljust 1000
    }

instance Monoid Config where
    mempty = Config
        { cfgNumThreads    = mempty
        }

    mappend a b = Config
        { cfgNumThreads = app cfgNumThreads a b
        }
      where app = on mappend

defaultOptions :: [OptDescr (IO Config)]
defaultOptions = [
      Option ['n'] ["threads"]
          (ReqArg (positive "number of threads" $ \n ->
               mempty { cfgNumThreads = n }) "N")
          "number of threads"
    ]

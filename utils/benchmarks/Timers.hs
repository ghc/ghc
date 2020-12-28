-- Benchmark that registers N timeouts, adjusts them a number of time
-- and finally waits for them to expire.

import Args (ljust, parseArgs, nonNegative, positive, theLast)
import Control.Concurrent (MVar, forkIO, takeMVar, newEmptyMVar, putMVar)
import Control.Monad (forM_, replicateM, when)
import Data.Function (on)
import Data.IORef (IORef, atomicModifyIORef, newIORef)
import Data.Monoid (Monoid(..), Last(..))
import System.Event (loop, new, registerTimeout, updateTimeout)
import System.Console.GetOpt (ArgDescr(ReqArg), OptDescr(..))
import System.Environment (getArgs)

data Config = Config
    { cfgNumTimeouts :: Last Int
    , cfgNumAdjusts  :: Last Int
    }

defaultConfig :: Config
defaultConfig = Config
    { cfgNumTimeouts = ljust 1000
    , cfgNumAdjusts  = ljust 3
    }

instance Monoid Config where
    mempty = Config
        { cfgNumTimeouts = mempty
        , cfgNumAdjusts  = mempty
        }

    mappend a b = Config
        { cfgNumTimeouts = app cfgNumTimeouts a b
        , cfgNumAdjusts  = app cfgNumAdjusts a b
        }
      where app = on mappend

defaultOptions :: [OptDescr (IO Config)]
defaultOptions = [
      Option ['n'] ["timeouts"]
          (ReqArg (positive "number of timeouts" $ \n ->
               mempty { cfgNumTimeouts = n }) "N")
          "number of timeouts to use"
    , Option ['a'] ["adjustments"]
          (ReqArg (nonNegative "number of adjustments" $ \n ->
               mempty { cfgNumAdjusts = n }) "N")
          "number of adjustments to use for each timeout"
    ]

callback :: MVar () -> IORef Int -> Config -> IO ()
callback done nref cfg = do
    a <- atomicModifyIORef nref (\a -> let !b = a+1 in (b,b))
    when (a >= numTimeouts) $ putMVar done ()
  where
    numTimeouts = theLast cfgNumTimeouts cfg

main :: IO ()
main = do
    (cfg, _) <- parseArgs defaultConfig defaultOptions =<< getArgs
    let numTimeouts = theLast cfgNumTimeouts cfg
        numAdjusts = theLast cfgNumAdjusts cfg

    mgr <- new
    forkIO $ loop mgr
    nref <- newIORef 0
    done <- newEmptyMVar
    let finalTimeout = 1  -- ms
        tenSecs = 10 * 1000  -- ms
        timeouts = replicate numAdjusts tenSecs ++ [finalTimeout]
        firstTimeout = head timeouts
    keys <- replicateM numTimeouts $ registerTimeout mgr firstTimeout
            (callback done nref cfg)
    forM_ (tail timeouts) $ \t ->
        forM_ keys $ \key -> updateTimeout mgr key t
    takeMVar done

-- Flow:
--
-- 1. Create N pipes.
--
-- Modelled after:
-- http://levent.svn.sourceforge.net/viewvc/levent/trunk/libevent/test/bench.c

import Args (ljust, parseArgs, positive, theLast)
import Control.Concurrent (MVar, forkIO, takeMVar, newEmptyMVar, putMVar)
import Control.Monad (forM_, replicateM, when)
import Data.Function (on)
import Data.IORef (IORef, atomicModifyIORef, newIORef)
import Data.Monoid (Monoid(..), Last(..))
import Foreign.C.Error (throwErrnoIfMinus1Retry, throwErrnoIfMinus1Retry_)
import Foreign.Marshal.Alloc (alloca)
import System.Console.GetOpt (ArgDescr(ReqArg), OptDescr(..))
import System.Environment (getArgs)
import System.Event (Event, EventManager, FdKey(..), evtRead, evtWrite, loop,
                     new, registerFd, registerTimeout)
import System.Event.Manager (newDefaultBackend, newWith)
import qualified System.Event.Poll as Poll
import System.Posix.IO (createPipe)
import System.Posix.Resource (ResourceLimit(..), ResourceLimits(..),
                              Resource(..), setResourceLimit)
import System.Posix.Internals (c_close, c_read, c_write)
import System.Posix.Types (Fd(..))

data Config = Config {
      cfgDelay :: Last Int
    , cfgNumPipes :: Last Int
    , cfgNumMessages :: Last Int
    }

defaultConfig :: Config
defaultConfig = Config {
                  cfgDelay       = ljust 0
                , cfgNumPipes    = ljust 448
                , cfgNumMessages = ljust 1024
                }

instance Monoid Config where
    mempty  = Config {
                cfgDelay = mempty
              , cfgNumPipes = mempty
              , cfgNumMessages = mempty
              }
    mappend a b = Config {
                    cfgDelay = app cfgDelay a b
                  , cfgNumPipes = app cfgNumPipes a b
                  , cfgNumMessages = app cfgNumMessages a b
                  }
        where app = on mappend

defaultOptions :: [OptDescr (IO Config)]
defaultOptions = [
  Option ['d'] ["delay"]
         (ReqArg (positive "delay in ms before read" $ \n -> mempty { cfgDelay = n }) "N")
          "number of pipes to use"
 ,Option ['p'] ["pipes"]
         (ReqArg (positive "number of pipes" $ \n -> mempty { cfgNumPipes = n }) "N")
          "number of pipes to use"
 ,Option ['m'] ["messages"]
         (ReqArg (positive "number of messages" $ \n -> mempty { cfgNumMessages = n }) "N")
          "number of messages to send"
 ]

readCallback :: Config -> EventManager -> MVar () -> IORef Int
             -> FdKey -> Event -> IO ()
readCallback cfg mgr done ref reg _ = do
  let numMessages = theLast cfgNumMessages cfg
      delay       = theLast cfgDelay cfg
      fd          = keyFd reg
  a <- atomicModifyIORef ref (\a -> let !b = a+1 in (b,b))
  case undefined of
    _ | a > numMessages -> close fd >> putMVar done ()
      | delay == 0      -> readByte fd
      | otherwise       -> registerTimeout mgr delay (readByte fd) >> return ()

writeCallback :: Config -> IORef Int -> FdKey -> Event -> IO ()
writeCallback cfg ref reg _ = do
  let numMessages = theLast cfgNumMessages cfg
      fd = keyFd reg
  a <- atomicModifyIORef ref (\a -> let !b = a+1 in (b,b))
  if a > numMessages
    then close fd
    else writeByte fd

main :: IO ()
main = do
    (cfg, _args) <- parseArgs defaultConfig defaultOptions =<< getArgs
    let numPipes = theLast cfgNumPipes cfg
        lim = ResourceLimit $ fromIntegral numPipes * 2 + 50
    setResourceLimit ResourceOpenFiles
        ResourceLimits { softLimit = lim, hardLimit = lim }

    putStrLn "creating pipes"
    pipePairs <- replicateM numPipes createPipe

    mgr <- newWith =<< newDefaultBackend
    -- mgr <- newWith =<< Poll.new
    _ <- forkIO $ loop mgr
    rref <- newIORef 0
    wref <- newIORef 0
    done <- newEmptyMVar
    putStrLn "registering readers"
    forM_ pipePairs $ \(r,_) ->
      registerFd mgr (readCallback cfg mgr done rref) r evtRead
    putStrLn "registering writers"
    forM_ pipePairs $ \(_,w) ->
      registerFd mgr (writeCallback cfg wref) w evtWrite
    putStrLn "waiting until done"
    takeMVar done

readByte :: Fd -> IO ()
readByte (Fd fd) =
    alloca $ \p -> throwErrnoIfMinus1Retry_ "readByte" $ c_read fd p 1

writeByte :: Fd -> IO ()
writeByte (Fd fd) =
    alloca $ \p -> do
      n <- throwErrnoIfMinus1Retry "writeByte" $ c_write fd p 1
      when (n /= 1) . error $ "writeByte returned " ++ show n

close :: Fd -> IO ()
close (Fd fd) = c_close fd >> return ()

{-# LANGUAGE BangPatterns #-}
-- Flow:
--
-- 1. Create N pipes.
--
-- Modelled after:
-- http://levent.svn.sourceforge.net/viewvc/levent/trunk/libevent/test/bench.c

import Args (ljust, parseArgs, positive, theLast)
import Control.Concurrent (MVar, forkIO, takeMVar, newEmptyMVar, putMVar)
import Control.Monad (forM_, replicateM, when, void)
import Data.IORef (IORef, atomicModifyIORef, newIORef)
import Data.Monoid (Last(..))
import Foreign.C.Error (throwErrnoIfMinus1Retry, throwErrnoIfMinus1Retry_)
import Foreign.Marshal.Alloc (alloca)
import System.Console.GetOpt (ArgDescr(ReqArg), OptDescr(..))
import System.Environment (getArgs)
import GHC.Event (Event, Lifetime(OneShot))
import qualified GHC.Event as ET (TimerManager, newWith, newDefaultBackend,
                                    registerTimeout)
import qualified GHC.Event as EM (FdKey, loop, keyFd, new, registerFd,
                                            evtRead, evtWrite)
import Data.Semigroup as Sem hiding (Last, Option)
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

instance Sem.Semigroup Config where
    (Config a b c) <>
      (Config d e f) =
      Config
      (a <> d)
      (b <> e)
      (c <> f)

instance Monoid Config where
    mempty  = Config mempty mempty mempty
    mappend = (<>)

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

readCallback :: Config -> ET.TimerManager -> MVar () -> IORef Int
             -> EM.FdKey -> Event -> IO ()
readCallback cfg mgr done ref reg _ = do
  let numMessages = theLast cfgNumMessages cfg
      delay       = theLast cfgDelay cfg
      fd          = EM.keyFd reg
  a <- atomicModifyIORef ref (\a -> let !b = a+1 in (b,b))
  case undefined of
    _ | a > numMessages -> close fd >> putMVar done ()
      | delay == 0      -> readByte fd
      | otherwise       -> void (ET.registerTimeout mgr delay (readByte fd))

writeCallback :: Config -> IORef Int -> EM.FdKey -> Event -> IO ()
writeCallback cfg ref reg _ = do
  let numMessages = theLast cfgNumMessages cfg
      fd = EM.keyFd reg
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

    mgr <- EM.new
    mgr_timer <- ET.newWith =<< ET.newDefaultBackend
    _ <- forkIO $ EM.loop mgr
    rref <- newIORef 0
    wref <- newIORef 0
    done <- newEmptyMVar
    putStrLn "registering readers"
    forM_ pipePairs $ \(r,_) ->
      EM.registerFd mgr (readCallback cfg mgr_timer done rref) r EM.evtRead OneShot
    putStrLn "registering writers"
    forM_ pipePairs $ \(_,w) ->
      EM.registerFd mgr (writeCallback cfg wref) w EM.evtWrite OneShot
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
close (Fd fd) = void (c_close fd)

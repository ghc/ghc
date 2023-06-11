{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Control.Concurrent      (ThreadId, forkIO, killThread,
                                          threadDelay)
import           Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import           Control.Exception       (Exception (..), SomeAsyncException,
                                          SomeException, bracket, catch, handle,
                                          throwIO)
import           Control.Monad           (forM_, unless)
import           GHC.Conc.IO             (threadWaitRead, threadWaitWrite)

import qualified Data.ByteString         as BS
import           GHC.IO.FD               (FD (..))
import           GHC.IO.Handle.FD        (handleToFd)
import           System.Environment      (getArgs)
import           System.IO               (BufferMode (NoBuffering), Handle,
                                          hClose, hSetBuffering)
import           System.IO.Error         (isResourceVanishedError)
import           System.Posix            (Fd (..))
import           System.Posix.IO         (createPipe, fdToHandle)

main :: IO ()
main = do
  [iterations] <- getArgs
  -- Usually 1000-2000 tries are enough to lock up, do even more just in case
  forM_ [1 :: Int .. read iterations] $ \_ -> do
    bracket setupPipes closePipes $ \(readH, writeH) -> do
      let
        handler e
          | Just as <- fromException e = throwIO (as :: SomeAsyncException)
          | otherwise = pure ()

      withThread (writer writeH) $ \_ ->
        reader readH `catch` handler


-- | Return a @(read, write)@ handle pair for an anonymous pipe.
setupPipes :: IO (Handle, Handle)
setupPipes = do
  (readFd, writeFd) <- createPipe
  (,)
    <$> fdToHandle readFd -- fdToHandle' readFd Nothing True ("read fd " <> show readFd) ReadMode True
    <*> fdToHandle writeFd -- fdToHandle' writeFd Nothing True ("write fd " <> show writeFd) WriteMode True

-- | Close the handles returned by 'setupPipes'.
closePipes :: (Handle, Handle) -> IO ()
closePipes (readH, writeH) = do
  dropResourceVanishedError $ hClose readH
  dropResourceVanishedError $ hClose writeH

reader :: Handle -> IO ()
reader readHandle = do
  let
    drain = do
      fd <- handleToFd readHandle
      threadWaitRead (Fd (fdFD fd))
      msg <- BS.hGetSome readHandle 1024
      unless (BS.null msg) drain

  -- The MVar ensures we more or less simultaneously start reading and closing, increasing the
  -- chance of hitting the race condition
  readingBarrier <- newEmptyMVar
  _ <- forkIO $ do
    takeMVar readingBarrier
    hClose readHandle `catch` \(_ :: SomeException) -> pure ()

  putMVar readingBarrier ()
  drain

withThread :: IO () -> (ThreadId -> IO r) -> IO r
withThread bgAction mainAction = do
  bracket
    (forkIO bgAction)
    killThread
    mainAction

-- | Something to keep the reader busy reading.
writer :: Handle -> IO ()
writer writeHandle = do
  hSetBuffering writeHandle NoBuffering
  let
    loop = do
      fd <- handleToFd writeHandle
      threadWaitWrite (Fd (fdFD fd))
      BS.hPut writeHandle $ BS.replicate 1024 65
      -- We need a short delay so that the reader actually needs to wait for data to be present.
      -- Only then can we trigger the epoll registration race condition.
      threadDelay 10_000
      loop

  dropResourceVanishedError loop

-- | Ignore broken pipe errors
dropResourceVanishedError :: IO () -> IO ()
dropResourceVanishedError = handle $ \err ->
  if isResourceVanishedError err
  then pure ()
  else throwIO err

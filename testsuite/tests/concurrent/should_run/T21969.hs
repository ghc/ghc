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


-- | Return a @(read, write)@ pair for an anonymous pipe, each as the handle
-- and the descriptor it was made from.
--
-- The descriptor is carried along rather than recovered later with
-- 'GHC.IO.Handle.FD.handleToFd', which rejects a 'DuplexHandle' -- and on
-- FreeBSD, whose pipes are bidirectional, that is what 'fdToHandle' makes of
-- these.  See #25512 for the symptom and #27889 for the cause.  It is the
-- same descriptor either way.
setupPipes :: IO ((Handle, Fd), (Handle, Fd))
setupPipes = do
  (readFd, writeFd) <- createPipe
  readH  <- fdToHandle readFd
  writeH <- fdToHandle writeFd
  return ((readH, readFd), (writeH, writeFd))

-- | Close the handles returned by 'setupPipes'.
closePipes :: ((Handle, Fd), (Handle, Fd)) -> IO ()
closePipes ((readH, _), (writeH, _)) = do
  dropResourceVanishedError $ hClose readH
  dropResourceVanishedError $ hClose writeH

reader :: (Handle, Fd) -> IO ()
reader (readHandle, readFd) = do
  let
    drain = do
      threadWaitRead readFd
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
writer :: (Handle, Fd) -> IO ()
writer (writeHandle, writeFd) = do
  hSetBuffering writeHandle NoBuffering
  let
    loop = do
      threadWaitWrite writeFd
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

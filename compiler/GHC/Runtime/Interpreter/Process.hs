module GHC.Runtime.Interpreter.Process
  (
  -- * Message API
    Message(..)
  , sendMessage
  )
where

import GHC.Prelude

import GHC.Runtime.Interpreter.Types
import GHCi.Message

import GHC.IO (catchException)
import GHC.Utils.Panic
import GHC.Utils.Exception as Ex

import Data.Binary
import System.Exit
import System.Process
import Data.Bifunctor

-- -----------------------------------------------------------------------------
-- Top-level Message API

-- | Send a message to the interpreter and expect a response, blocking until it arrives.
sendMessage :: Binary a => ExtInterpInstance d -> Message a -> IO a
sendMessage i msg = unwrapRight =<<
  remoteCall (interpPipe proc) msg
    `catchException` \(e :: SomeException) -> handleInterpProcessFailure proc e
  where
    proc = instProcess i
    unwrapRight :: Either SomeException b -> IO b
    unwrapRight (Right a) = pure a
    unwrapRight (Left er) = do
      let ex = RemoteCallFailed er
      unwrapRight . bimap (addExceptionContext (WhileHandling (SomeException ex))) id
        =<< remoteCall (interpPipe proc) Shutdown
      throwIO ex

handleInterpProcessFailure :: InterpProcess -> SomeException -> IO a
handleInterpProcessFailure i e = do
  let hdl = interpHandle i
  ex <- getProcessExitCode hdl
  case ex of
    Just (ExitFailure n) ->
      throwIO (InstallationError ("External interpreter terminated (" ++ show n ++ ")"))
    _ -> do
      terminateProcess hdl
      _ <- waitForProcess hdl
      throw e

-- | The external interpreter server threw an exception while executing this
-- remote call. The ext. interp is still live and ready to take further
-- commands (such as 'Shutdown').
data RemoteCallFailed = RemoteCallFailed SomeException deriving Show
instance Exception RemoteCallFailed where
  displayException (RemoteCallFailed e)
    = "A remote call in the external interpreter threw an exception:\n"
    ++ displayException e

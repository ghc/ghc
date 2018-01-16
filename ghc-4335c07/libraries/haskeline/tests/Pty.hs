{-# LANGUAGE ForeignFunctionInterface #-}
-- This module is a quick-and-dirty way to run an executable
-- within a pseudoterminal and obtain its output.
-- 
-- It is not intended for general use.  In particular:
-- - It expects the output to be available quickly (<0.2s)
--   after each chunk of input.
-- - It expects each output chunk be no more than 4096 bytes.
module Pty (runCommandInPty) where

import System.Posix.Types
import System.Posix.Terminal
import System.Posix.Process
import System.Posix.Signals
import qualified Data.ByteString as B
import System.Posix.IO.ByteString
import Foreign.Marshal.Alloc
import Foreign.C.Error
import Foreign.C.Types
import Foreign.Ptr
import Control.Exception
import Control.Concurrent
import Control.Monad (liftM2)

-- Run the given command in a pseudoterminal, and return its output chunks.
-- Read the initial output, then feed the given input to it
-- one block at a time.
-- After each input, pause for 0.2s and then read as much output as possible.
runCommandInPty :: String -> [String] -> Maybe [(String,String)]
        -> [B.ByteString] -> IO [B.ByteString]
runCommandInPty prog args env inputs = do
    (fd,pid) <- forkCommandInPty prog args env
    -- Block until the initial output from the program.
    -- After that, only use non-blocking reads.  Otherwise,
    -- we would hang the program in cases where the program has no output
    -- in between inputs.
    firstOutput <- getOutput fd
    setFdOption fd NonBlockingRead True
    outputs <- mapM (inputOutput fd) inputs
    signalProcess killProcess pid
    status <- getProcessStatus True False pid
    closeFd fd
    return (firstOutput : outputs)

inputOutput :: Fd -> B.ByteString -> IO B.ByteString
inputOutput fd input = do
    putInput fd input
    getOutput fd

putInput :: Fd -> B.ByteString -> IO ()
putInput fd input =
    B.useAsCStringLen input $ \(cstr,len) -> do
        written <- fdWriteBuf fd (castPtr cstr) (fromIntegral len)
        if written == 0
            then threadDelay 1000 >> putInput fd input
            else return ()

getOutput :: Fd -> IO B.ByteString
getOutput fd = do
            threadDelay 20000
            allocaBytes (fromIntegral numBytes) $ \buf -> do
                num <- fdReadNonBlocking fd buf numBytes
                B.packCStringLen (castPtr buf, fromIntegral num)
  where
    numBytes = 4096


-- Unlike fdReadBuf, don't throw an error if nothing's immediately available.
-- Instead, just return empty output.
fdReadNonBlocking :: Fd -> Ptr CChar -> CSize -> IO CSsize
fdReadNonBlocking fd buf n = do
    num_read <- c_read fd buf n
    if num_read >= 0
        then return num_read
        else do
            e <- getErrno
            if e == eAGAIN
                then return 0
                else throwErrno "fdReadNonBlocking"

foreign import ccall "read" c_read :: Fd -> Ptr CChar -> CSize -> IO CSsize


-- returns the master Fd, and the pid for the subprocess.
forkCommandInPty :: String -> [String] -> Maybe [(String,String)]
                    -> IO (Fd,ProcessID)
forkCommandInPty prog args env = do
    (master,slave) <- openPseudoTerminal
    pid <- forkProcess $ do
                    closeFd master
                    loginTTY slave
                    executeFile prog False args env
    return (master,pid)


loginTTY :: Fd -> IO ()
loginTTY = throwErrnoIfMinus1_ "loginTTY" . login_tty

foreign import ccall login_tty :: Fd -> IO CInt


{-# LANGUAGE ScopedTypeVariables #-}
-- A process may be started with stdout closed -- `prog >&-` from a shell does
-- exactly that.  Nothing then reserves descriptor 1, so the next descriptor
-- the runtime opens for itself takes it, and the 'Handle' for stdout, built
-- later when the program first writes, takes over a descriptor belonging to
-- the runtime.  The write then goes into the runtime's own plumbing, and what
-- this looks like from the outside is a hang.  See #24450 and
-- Note [Standard file descriptors must be open] in rts/RtsStartup.c.
--
-- This test is run with its stdin and stdout closed, so what it has to say
-- goes to stderr.  The four writes are the reproducer from the ticket and must
-- simply finish.  The lines after them are what tell the two outcomes apart:
-- the descriptors the runtime opens for itself are pipes, so finding a pipe on
-- descriptor 0 or 1 means it was handed to the runtime.
module Main (main) where

import Control.Exception (IOException, try)
import System.IO
import System.Posix.Files (getFdStatus, isCharacterDevice, isNamedPipe)
import System.Posix.Types (Fd (..))

main :: IO ()
main = do
    hPutChar stderr 'A'     -- unbuffered
    hPutChar stdout 'X'     -- buffered; this is the one that used to hang
    hPutChar stderr 'Z'     -- unbuffered
    hPutChar stderr '\n'    -- unbuffered
    mapM_ (\fd -> hPutStrLn stderr =<< describeFd fd) [0, 1]

describeFd :: Fd -> IO String
describeFd fd = do
    r <- try (getFdStatus fd)
    return $ "fd " ++ show fd ++ ": " ++ case r of
      Left (_ :: IOException) -> "closed"
      Right st
        -- What the fix arranges: /dev/null, opened onto the free number
        -- before the runtime could take it.
        | isCharacterDevice st -> "character device"
        -- What #24450 is: the runtime's ticker or control pipe.
        | isNamedPipe st       -> "pipe"
        | otherwise            -> "something else"

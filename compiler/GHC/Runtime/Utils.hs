module GHC.Runtime.Utils
  ( runWithPipes
  )
where

import GHC.Prelude
import GHC.Utils.Exception (onException)

import System.Process
import System.Process.CommunicationHandle
import System.IO

-- | Spawn a process connected to us by two pipes. Important to close our copy
-- of the endpoints passed to the child so each side only keeps open copies it
-- needs. If we don't do that, then closing one end wouldn't interrupt reads on
-- the other.
runWithPipes :: (CreateProcess -> IO ProcessHandle)
             -> FilePath -> [String] -> [String] -> IO (ProcessHandle, Handle, Handle)
runWithPipes createProc prog pre_opts opts = do
    (rh, chTheyWrite) <- createWeReadTheyWritePipe
    (chTheyRead, wh)  <- createTheyReadWeWritePipe
    let args = pre_opts ++ (show chTheyWrite : show chTheyRead : opts)
        closeTheirs = do
          closeCommunicationHandle chTheyWrite
          closeCommunicationHandle chTheyRead
    -- These handles have no finalizers, so close them ourselves if the spawn fails.
    ph <- createProc (proc prog args)
            `onException` (closeTheirs >> hClose rh >> hClose wh)
    closeTheirs
    return (ph, rh, wh)

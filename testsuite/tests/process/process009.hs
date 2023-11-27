import Control.Monad
import System.Exit
import System.Process
import Data.Maybe
import Data.List (intercalate)

-- Test that we get the right exit code for processes that terminate
-- with a signal (#7229)

main = do
  let script = intercalate " "
               [ "exec python3 2>/dev/null"
               , "-c"
               , "'import os; os.kill(os.getpid(), 1)'"
               ]
  (_,_,_,p) <- createProcess (shell script)
  waitForProcess p >>= print
  getProcessExitCode p >>= print

  (_,_,_,p) <- createProcess (shell script)
  forever $ do
    r <- getProcessExitCode p
    if (isJust r) then do print r; exitWith ExitSuccess else return ()


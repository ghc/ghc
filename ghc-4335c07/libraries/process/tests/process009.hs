import Control.Monad
import System.Exit
import System.Process
import Data.Maybe

-- Test that we get the right exit code for processes that terminate
-- with a signal (#7229)

main = do
  (_,_,_,p) <- createProcess (shell "kill -HUP $$")
  waitForProcess p >>= print
  getProcessExitCode p >>= print

  (_,_,_,p) <- createProcess (shell "kill -HUP $$")
  forever $ do
    r <- getProcessExitCode p
    if (isJust r) then do print r; exitWith ExitSuccess else return ()


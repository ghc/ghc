
-- trac #2508

import System.Exit
import Control.OldException

main = exitWith ExitSuccess `finally` return ()

-- Used to present a consistent shell view for :! commands in GHCi
-- scripts.  We're assuming that sh is in the path and that it
-- is a Bourne-compatible shell.

import System.Cmd
import System.Exit

shell :: String -> IO ExitCode
shell s = rawSystem "sh" ["-c", s]

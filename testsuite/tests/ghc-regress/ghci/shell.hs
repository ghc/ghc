{-# OPTIONS -cpp #-}

-- Used to present a consistent shell view for :! commands in GHCi
-- scripts.  We're assuming that sh.exe is in the path and that it
-- is a Bourne-compatible shell.

import System.Cmd
#ifdef mingw32_HOST_OS
shell s = system ("sh.exe -c '" ++ s ++ "'")
#else
shell s = system s
#endif

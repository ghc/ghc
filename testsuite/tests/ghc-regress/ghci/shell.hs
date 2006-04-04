{-# OPTIONS -cpp #-}

-- Used to present a consistent shell view for :! commands in GHCi
-- scripts.  We're assuming cygwin is installed in C:/cygwin.

import System.Cmd
#ifdef mingw32_HOST_OS
shell s = system ("c:/cygwin/bin/sh.exe -c '" ++ s ++ "'")
#else
shell s = system s
#endif

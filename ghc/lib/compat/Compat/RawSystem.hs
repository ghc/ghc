{-# OPTIONS -cpp #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Compat.RawSystem
-- Copyright   :  (c) The University of Glasgow 2001-2004
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- This is an implementation of rawSystem for use on older versions of GHC
-- which had missing or buggy implementations of this function.
--
-----------------------------------------------------------------------------

module Compat.RawSystem (rawSystem) where

#include "../../includes/ghcconfig.h"

#if __GLASGOW_HASKELL__ >= 603

import System.Cmd (rawSystem)

#else /* to end of file */

import System.Exit
import Foreign
import Foreign.C

{- | 
The computation @'rawSystem' cmd args@ runs the operating system command
whose file name is @cmd@, passing it the arguments @args@.  It
bypasses the shell, so that @cmd@ should see precisely the argument
strings @args@, with no funny escaping or shell meta-syntax expansion.
(Unix users will recognise this behaviour 
as @execvp@, and indeed that's how it's implemented.)
It will therefore behave more portably between operating systems than 'system'.

The return codes are the same as for 'system'.
-}

rawSystem :: FilePath -> [String] -> IO ExitCode

{- -------------------------------------------------------------------------
 	IMPORTANT IMPLEMENTATION NOTES
   (see also libraries/base/cbits/rawSystem.c)

On Unix, rawSystem is easy to implement: use execvp.

On Windows it's more tricky.  We use CreateProcess, passing a single
command-line string (lpCommandLine) as its argument.  (CreateProcess
is well documented on http://msdn.microsoft/com.)

  - It parses the beginning of the string to find the command. If the
	file name has embedded spaces, it must be quoted, using double
	quotes thus 
		"foo\this that\cmd" arg1 arg2

  - The invoked command can in turn access the entire lpCommandLine string,
	and the C runtime does indeed do so, parsing it to generate the 
	traditional argument vector argv[0], argv[1], etc.  It does this
	using a complex and arcane set of rules which are described here:
	
	   http://msdn.microsoft.com/library/default.asp?url=/library/en-us/vccelng/htm/progs_12.asp

	(if this URL stops working, you might be able to find it by
	searching for "Parsing C Command-Line Arguments" on MSDN.  Also,
	the code in the Microsoft C runtime that does this translation
	is shipped with VC++).


Our goal in rawSystem is to take a command filename and list of
arguments, and construct a string which inverts the translatsions
described above, such that the program at the other end sees exactly
the same arguments in its argv[] that we passed to rawSystem.

This inverse translation is implemented by 'translate' below.

Here are some pages that give informations on Windows-related 
limitations and deviations from Unix conventions:

    http://support.microsoft.com/default.aspx?scid=kb;en-us;830473
    Command lines and environment variables effectively limited to 8191 
    characters on Win XP, 2047 on NT/2000 (probably even less on Win 9x):

    http://www.microsoft.com/windowsxp/home/using/productdoc/en/default.asp?url=/WINDOWSXP/home/using/productdoc/en/percent.asp
    Command-line substitution under Windows XP. IIRC these facilities (or at 
    least a large subset of them) are available on Win NT and 2000. Some 
    might be available on Win 9x.

    http://www.microsoft.com/windowsxp/home/using/productdoc/en/default.asp?url=/WINDOWSXP/home/using/productdoc/en/Cmd.asp
    How CMD.EXE processes command lines.


Note: CreateProcess does have a separate argument (lpApplicationName)
with which you can specify the command, but we have to slap the
command into lpCommandLine anyway, so that argv[0] is what a C program
expects (namely the application name).  So it seems simpler to just
use lpCommandLine alone, which CreateProcess supports.

----------------------------------------------------------------------------- -}

#ifndef mingw32_HOST_OS

rawSystem cmd args =
  withCString cmd $ \pcmd ->
    withMany withCString (cmd:args) $ \cstrs ->
      withArray0 nullPtr cstrs $ \arr -> do
	status <- throwErrnoIfMinus1 "rawSystem" (c_rawSystem pcmd arr)
        case status of
            0  -> return ExitSuccess
            n  -> return (ExitFailure n)

foreign import ccall unsafe "rawSystem"
  c_rawSystem :: CString -> Ptr CString -> IO Int

#else

-- On Windows, the command line is passed to the operating system as
-- a single string.  Command-line parsing is done by the executable
-- itself.
rawSystem cmd args = do
	-- NOTE: 'cmd' is assumed to contain the application to run _only_,
	-- as it'll be quoted surrounded in quotes here.
  let cmdline = translate cmd ++ concat (map ((' ':) . translate) args)
  withCString cmdline $ \pcmdline -> do
    status <- throwErrnoIfMinus1 "rawSystem" (c_rawSystem pcmdline)
    case status of
       0  -> return ExitSuccess
       n  -> return (ExitFailure n)

translate :: String -> String
translate str@('"':_) = str -- already escaped.
	-- ToDo: this case is wrong.  It is only here because we
	-- abuse the system in GHC's SysTools by putting arguments into
	-- the command name; at some point we should fix it up and remove
	-- the case above.
translate str = '"' : snd (foldr escape (True,"\"") str)
  where escape '"'  (b,     str) = (True,  '\\' : '"'  : str)
        escape '\\' (True,  str) = (True,  '\\' : '\\' : str)
        escape '\\' (False, str) = (False, '\\' : str)
	escape c    (b,     str) = (False, c : str)
	-- See long comment above for what this function is trying to do.
	--
	-- The Bool passed back along the string is True iff the
	-- rest of the string is a sequence of backslashes followed by
	-- a double quote.

foreign import ccall unsafe "rawSystem"
  c_rawSystem :: CString -> IO Int

#endif

#endif


-----------------------------------------------------------------------------
-- |
-- Module      :  System.Cmd
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Executing an external command.
--
-----------------------------------------------------------------------------

module System.Cmd
    ( system,        -- :: String -> IO ExitCode
#ifdef __GLASGOW_HASKELL__
      rawSystem,     -- :: FilePath -> [String] -> IO ExitCode
#endif
    ) where

import Prelude

#ifdef __GLASGOW_HASKELL__
import Foreign
import Foreign.C
import System.Exit
import GHC.IOBase
#include "config.h"
#endif

#ifdef __HUGS__
import Hugs.System
#endif

#ifdef __NHC__
import System (system)
#endif

-- ---------------------------------------------------------------------------
-- system

{-| 
Computation @system cmd@ returns the exit code
produced when the operating system processes the command @cmd@.

This computation may fail with

   * @PermissionDenied@: The process has insufficient privileges to
     perform the operation.

   * @ResourceExhausted@: Insufficient resources are available to
     perform the operation.

   * @UnsupportedOperation@: The implementation does not support
     system calls.

On Windows, 'system' is implemented using Windows's native system
call, which ignores the @SHELL@ environment variable, and always
passes the command to the Windows command interpreter (@CMD.EXE@ or
@COMMAND.COM@), hence Unixy shell tricks will not work.
-}
#ifdef __GLASGOW_HASKELL__
system :: String -> IO ExitCode
system "" = ioException (IOError Nothing InvalidArgument "system" "null command" Nothing)
system cmd =
  withCString cmd $ \s -> do
    status <- throwErrnoIfMinus1 "system" (primSystem s)
    case status of
        0  -> return ExitSuccess
        n  -> return (ExitFailure n)

foreign import ccall unsafe "systemCmd" primSystem :: CString -> IO Int


------------------------------------------------------------------------
--
--			rawSystem
--
------------------------------------------------------------------------

{- | 
The computation @rawSystem cmd args@ runs the operating system command
whose file name is @cmd@, passing it the arguments @args@.  It
bypasses the shell, so that @cmd@ should see precisely the argument
strings @args@, with no funny escaping or shell meta-syntax expansion.
(Unix users will recognise this behaviour 
as @execvp@, and indeed that's how it's implemented.)
It will therefore behave more portably between operating systems than @system@.

The return codes are the same as for @system@.
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
	traditional argument vector argv[0], argv[1], etc.  Again, to
	break it into argument items, any spaces must be quoted using
	double quote thus
		cmd "this is arg 1" "this is arg 2"

What if an argument itself contains double-quotes? (File names can't
can't, on Windows.)  Then the quote must be escaped with a backslash.
If we call Create Process with this lpArgument:
	cmd "Foo=\"baz\"" arg2
then cmd will see argv[1] as
	Foo="baz"
However, experiments show that backslashes themselves must *not* be escaped.
That is, to get a backslash in an argument, just put backslash, even inside
quotes.  For eaxmple, this works fine to show the contents of the file
foo\baz
	cat "foo\baz"
If you escape the backslash, thus
	cat "foo\\baz"
then @cat@ will see argument foo\\baz, and on WinME/98/95 you'll get
"can't find file foo\\baz".  (As it happens, WinNT/XP commands don't
mind double backslashes, but it's still a bug, given rawSystem's claim
to pass exactly args to the command.)

BOTTOM LINE: 
	1 We wrap the command, and each argument, in quotes
	2 Inside the quotes, we escape any double-quote characters
		(but nothing else)
	3 Then concatenate all these quoted things together, separated with 
		spaces

Steps 1,2 are done by the function 'translate' below.

Question: how do you get the string \" into an argument?  Turns out that
the argument "\\"" does not do the job.  (This turns into a single \.)
Puzzling but probably not important in practice.

Note: CreateProcess does have a separate argument (lpApplicationName)
with which you can specify the command, but we have to slap the
command into lpCommandLine anyway, so that argv[0] is what a C program
expects (namely the application name).  So it seems simpler to just
use lpCommandLine alone, which CreateProcess supports.

----------------------------------------------------------------------------- -}

#ifndef mingw32_TARGET_OS

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
translate str = '"' : foldr escape "\"" str
  where escape '"'  str = '\\' : '"'  : str
	escape c    str = c : str

foreign import ccall unsafe "rawSystem"
  c_rawSystem :: CString -> IO Int

#endif

#endif  /* __GLASGOW_HASKELL__ */

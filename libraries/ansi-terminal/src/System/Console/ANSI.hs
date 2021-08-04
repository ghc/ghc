#include "Common-Safe-Haskell.hs"

{-| Through this module, this library provides platform-independent support for
control character sequences following the \'ANSI\' standards (see further below)
for terminal software that supports those sequences, running on a Unix-like
operating system or Windows.

The sequences of control characters (also referred to as \'escape\' sequences or
codes) provide a rich range of functionality for terminal control, which
includes:

 * Colored text output, with control over both foreground and background colors

 * Clearing parts of a line or the screen

 * Hiding or showing the cursor

 * Moving the cursor around

 * Reporting the position of the cursor

 * Scrolling the screen up or down

 * Changing the title of the terminal

A terminal that supports control character sequences acts on them when they
are flushed from the output buffer (with a newline character @\"\\n\"@ or, for
the standard output channel, @hFlush stdout@).

The functions moving the cursor to an absolute position are 0-based (the
top-left corner is considered to be at row 0 column 0) (see 'setCursorPosition')
and so is 'getCursorPosition'. The \'ANSI\' standards themselves are 1-based
(that is, the top-left corner is considered to be at row 1 column 1) and some
functions reporting the position of the cursor are too (see
'reportCursorPosition').

The native terminal software on Windows is \'Command Prompt\' or \`PowerShell\`.
Before Windows 10 version 1511 (known as the \'November [2015] Update\' or
\'Threshold 2\') that software did not support such control sequences. For that
software, this library also provides support for such sequences by using
emulation.

Terminal software other than the native software exists for Windows. One example
is the \'mintty\' terminal emulator for \'Cygwin\', \'MSYS\' or \'MSYS2\', and
dervied projects, and for \'WSL\' (Windows Subsystem for Linux).

The \'ANSI\' standards refer to (1) standard ECMA-48 \`Control Functions for
Coded Character Sets\' (5th edition, 1991); (2) extensions in ITU-T
Recommendation (previously CCITT Recommendation) T.416 (03/93) \'Information
Technology – Open Document Architecture (ODA) and Interchange Format: Character
Content Architectures\` (also published as ISO/IEC International Standard
8613-6); and (3) further extensions used by \'XTerm\', a terminal emulator for
the X Window System. The escape codes are described in a Wikipedia article at
<http://en.wikipedia.org/wiki/ANSI_escape_code> and those codes supported on
current versions of Windows at
<https://docs.microsoft.com/en-us/windows/console/console-virtual-terminal-sequences>.

The whole of the \'ANSI\' standards are not supported by this library but most
(if not all) of the parts that are popular and well-supported by terminal
software are supported. Every function exported by this module comes in three
variants, namely:

 * A variant that has an @IO ()@ type and doesn't take a @Handle@ (for example,
   @clearScreen :: IO ()@). This variant just outputs the \`ANSI\` command
   directly to the standard output channel ('stdout') and any terminal
   corresponding to it. Commands issued like this should work as you expect on
   both Unix-like operating systems and Windows.

 * An \'@h@...\' variant that has an @IO ()@ type but takes a @Handle@ (for
   example, @hClearScreen :: Handle -> IO ()@). This variant outputs the
   \`ANSI\` command to the supplied handle and any terminal corresponding to it.
   Commands issued like this should also work as you expect on both Unix-like
   operating systems and Windows.

 * A \'...@Code@\' variant that has a @String@ type (for example,
   @clearScreenCode :: String@). This variant outputs the sequence of control
   characters as a 'String', which can be added to any other bit of text before
   being output. The use of these codes is generally discouraged because they
   will not work on legacy versions of Windows where the terminal in use is not
   ANSI-enabled (see further above). On Windows, where emulation has been
   necessary, these variants will always output the empty string. That is done
   so that it is possible to use them portably; for example, coloring console
   output on the understanding that you will see colors only if you are running
   on a Unix-like operating system or a version of Windows where emulation has
   not been necessary. If the control characters are always required, see module
   "System.Console.ANSI.Codes".

Example:

> module Main where
>
> import System.Console.ANSI
>
> -- Set colors and write some text in those colors.
> main :: IO ()
> main = do
>   setSGR [SetColor Foreground Vivid Red]
>   setSGR [SetColor Background Vivid Blue]
>   putStrLn "Red-On-Blue"
>   setSGR [Reset]  -- Reset to default colour scheme
>   putStrLn "Default colors."

Another example:

> module Main where
>
> import System.IO (hFlush, stdout)
> import System.Console.ANSI
>
> main :: IO ()
> main = do
>   setSGR [SetColor Foreground Dull Blue]
>   putStr "Enter your name: "
>   setSGR [SetColor Foreground Dull Yellow]
>   hFlush stdout  -- flush the output buffer before getLine
>   name <- getLine
>   setSGR [SetColor Foreground Dull Blue]
>   putStrLn $ "Hello, " ++ name ++ "!"
>   setSGR [Reset]  -- reset to default colour scheme

For many more examples, see the project's extensive
<https://github.com/feuerbach/ansi-terminal/blob/master/app/Example.hs Example.hs> file.
-}
#if defined(WINDOWS)
module System.Console.ANSI
  (
    module System.Console.ANSI.Windows
  ) where

import System.Console.ANSI.Windows

#elif defined(UNIX)

module System.Console.ANSI
  (
    module System.Console.ANSI.Unix
  ) where

import System.Console.ANSI.Unix

#else

#error Unsupported platform for the ansi-terminal package

#endif

module System.Console.Haskeline.Backend where

import System.Console.Haskeline.Term
import System.Console.Haskeline.Monads
import Control.Monad
import System.IO (stdin, hGetEcho, Handle)

#ifdef MINGW
import System.Console.Haskeline.Backend.Win32 as Win32
#else
import System.Console.Haskeline.Backend.Posix as Posix
#ifdef TERMINFO
import System.Console.Haskeline.Backend.Terminfo as Terminfo
#endif
import System.Console.Haskeline.Backend.DumbTerm as DumbTerm
#endif


defaultRunTerm :: IO RunTerm
defaultRunTerm = (liftIO (hGetEcho stdin) >>= guard >> stdinTTY)
                    `orElse` fileHandleRunTerm stdin

terminalRunTerm :: IO RunTerm
terminalRunTerm = directTTY `orElse` fileHandleRunTerm stdin

stdinTTY :: MaybeT IO RunTerm
#ifdef MINGW
stdinTTY = win32TermStdin
#else
stdinTTY = stdinTTYHandles >>= runDraw
#endif

directTTY :: MaybeT IO RunTerm
#ifdef MINGW
directTTY = win32Term
#else
directTTY = ttyHandles >>= runDraw
#endif


#ifndef MINGW
runDraw :: Handles -> MaybeT IO RunTerm
#ifndef TERMINFO
runDraw = runDumbTerm
#else
runDraw h = runTerminfoDraw h `mplus` runDumbTerm h
#endif
#endif

fileHandleRunTerm :: Handle -> IO RunTerm
#ifdef MINGW
fileHandleRunTerm = Win32.fileRunTerm
#else
fileHandleRunTerm = Posix.fileRunTerm
#endif

\begin{code}
-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.TopHandler
-- Copyright   :  (c) The University of Glasgow, 2001-2002
-- License     :  see libraries/base/LICENSE
-- 
-- Maintainer  :  cvs-ghc@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- Support for catching exceptions raised during top-level computations
-- (e.g. @Main.main@, 'Control.Concurrent.forkIO', and foreign exports)
--
-----------------------------------------------------------------------------

-- #hide
module GHC.TopHandler (
   runMainIO, runIO, runNonIO, reportStackOverflow, reportError
  ) where

import Prelude

import System.IO
import Control.Exception

import GHC.IOBase
import GHC.Exception
import GHC.Prim (unsafeCoerce#)

-- | 'runMainIO' is wrapped around 'Main.main' (or whatever main is
-- called in the program).  It catches otherwise uncaught exceptions,
-- and also flushes stdout\/stderr before exiting.
runMainIO :: IO a -> IO a
runMainIO main = (do a <- main; cleanUp; return a) `catchException` topHandler

-- | 'runIO' is wrapped around every @foreign export@ and @foreign
-- import \"wrapper\"@ to mop up any uncaught exceptions.  Thus, the
-- result of running 'System.Exit.exitWith' in a foreign-exported
-- function is the same as in the main thread: it terminates the
-- program.
--
runIO :: IO a -> IO a
runIO main = catchException main topHandler

-- | The same as 'runIO', but for non-IO computations.  Used for
-- wrapping @foreign export@ and @foreign import \"wrapper\"@ when these
-- are used to export Haskell functions with non-IO types.
--
runNonIO :: a -> IO a
runNonIO a = catchException (a `seq` return a) topHandler

topHandler :: Exception -> IO a
topHandler err = catchException (real_handler err) topHandler

-- Make sure we handle errors while reporting the error!
-- (e.g. evaluating the string passed to 'error' might generate
--  another error, etc.)
--
real_handler :: Exception -> IO a
real_handler ex =
  cleanUp >>
  case ex of
	AsyncException StackOverflow -> do
	   reportStackOverflow
	   safeExit 2

	-- only the main thread gets ExitException exceptions
	ExitException ExitSuccess     -> safeExit 0
	ExitException (ExitFailure n) -> safeExit n

	other -> do
	   reportError other
	   safeExit 1
	   

reportStackOverflow :: IO a
reportStackOverflow = do callStackOverflowHook; return undefined

reportError :: Exception -> IO a
reportError ex = do
   handler <- getUncaughtExceptionHandler
   handler ex
   return undefined

-- SUP: Are the hooks allowed to re-enter Haskell land?  If so, remove
-- the unsafe below.
foreign import ccall unsafe "stackOverflow"
	callStackOverflowHook :: IO ()

-- try to flush stdout/stderr, but don't worry if we fail
-- (these handles might have errors, and we don't want to go into
-- an infinite loop).
cleanUp :: IO ()
cleanUp = do
  hFlush stdout `catchException` \_ -> return ()
  hFlush stderr `catchException` \_ -> return ()

cleanUpAndExit :: Int -> IO a
cleanUpAndExit r = do cleanUp; safeExit r

-- we have to use unsafeCoerce# to get the 'IO a' result type, since the
-- compiler doesn't let us declare that as the result type of a foreign export.
safeExit :: Int -> IO a
safeExit r = unsafeCoerce# (shutdownHaskellAndExit r)

-- NOTE: shutdownHaskellAndExit must be called "safe", because it *can*
-- re-enter Haskell land through finalizers.
foreign import ccall "shutdownHaskellAndExit" 
  shutdownHaskellAndExit :: Int -> IO ()
\end{code}

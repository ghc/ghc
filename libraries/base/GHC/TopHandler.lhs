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

module GHC.TopHandler (
   runIO, runNonIO, reportStackOverflow, reportError 
  ) where

import Prelude

import System.IO

import Foreign.C.String
import Foreign.Ptr
import GHC.IOBase
import GHC.Exception
import GHC.Prim (unsafeCoerce#)

-- | 'runIO' is wrapped around @Main.main@ by @TcModule@.  It is also wrapped
-- around every @foreign export@ and @foreign import \"wrapper\"@ to mop up
-- any uncaught exceptions.  Thus, the result of running
-- 'System.Exit.exitWith' in a foreign-exported function is the same as
-- in the main thread: it terminates the program.
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
  case ex of
	AsyncException StackOverflow -> reportStackOverflow True

	-- only the main thread gets ExitException exceptions
	ExitException ExitSuccess     -> safe_exit 0
	ExitException (ExitFailure n) -> safe_exit n

	Deadlock    -> reportError True 
			"no threads to run:  infinite loop or deadlock?"
  
	ErrorCall s -> reportError True s
	other       -> reportError True (showsPrec 0 other "\n")

reportStackOverflow :: Bool -> IO a
reportStackOverflow bombOut = do
   (hFlush stdout) `catchException` (\ _ -> return ())
   callStackOverflowHook
   if bombOut 
	then exit 2
	else return undefined

reportError :: Bool -> String -> IO a
reportError bombOut str = do
   (hFlush stdout) `catchException` (\ _ -> return ())
   withCStringLen str $ \(cstr,len) -> do
     writeErrString errorHdrHook cstr len
     if bombOut 
	then exit 1
        else return undefined

#ifndef ILX
foreign import ccall "&ErrorHdrHook" errorHdrHook :: Ptr ()
#else
foreign import ccall "ErrorHdrHook" errorHdrHook :: Ptr ()
#endif

foreign import ccall unsafe "writeErrString__"
	writeErrString :: Ptr () -> CString -> Int -> IO ()

-- SUP: Are the hooks allowed to re-enter Haskell land?  If so, remove
-- the unsafe below.
foreign import ccall unsafe "stackOverflow"
	callStackOverflowHook :: IO ()

foreign import ccall unsafe "stg_exit"
	stg_exit :: Int -> IO ()

exit :: Int -> IO a
exit r = unsafeCoerce# (stg_exit r)

-- NOTE: shutdownHaskellAndExit must be called "safe", because it *can*
-- re-enter Haskell land through finalizers.
foreign import ccall "shutdownHaskellAndExit" 
  shutdownHaskellAndExit :: Int -> IO ()

-- we have to use unsafeCoerce# to get the 'IO a' result type, since the
-- compiler doesn't let us declare that as the result type of a foreign export.
safe_exit :: Int -> IO a
safe_exit r = unsafeCoerce# (shutdownHaskellAndExit r)
\end{code}

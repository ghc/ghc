-- -----------------------------------------------------------------------------
-- $Id: TopHandler.lhs,v 1.5 2002/02/11 12:28:57 simonmar Exp $
--
-- (c) The University of Glasgow, 2001
--
-- GHC.TopHandler
--
-- 'Top-level' IO actions want to catch exceptions (e.g., forkIO and 
-- GHC.Main.mainIO) and report them - topHandler is the exception
-- handler they should use for this:

-- make sure we handle errors while reporting the error!
-- (e.g. evaluating the string passed to 'error' might generate
--  another error, etc.)

-- These functions can't go in GHC.Main, because GHC.Main isn't
-- included in HSstd.o (because GHC.Main depends on Main, which
-- doesn't exist yet...).

\begin{code}
module GHC.TopHandler (
   runMain, reportStackOverflow, reportError 
  ) where

import Prelude

import System.IO

import Foreign.C.String
import Foreign.Ptr
import GHC.IOBase
import GHC.Exception

-- runMain is applied to Main.main by TcModule
runMain :: IO a -> IO ()
runMain main = catchException (main >> return ()) topHandler
  
topHandler :: Exception -> IO ()
topHandler err = catchException (real_handler err) topHandler

real_handler :: Exception -> IO ()
real_handler ex =
  case ex of
	AsyncException StackOverflow -> reportStackOverflow True

	-- only the main thread gets ExitException exceptions
	ExitException ExitSuccess     -> shutdownHaskellAndExit 0
	ExitException (ExitFailure n) -> shutdownHaskellAndExit n

	Deadlock    -> reportError True 
			"no threads to run:  infinite loop or deadlock?"
  
	ErrorCall s -> reportError True s
	other       -> reportError True (showsPrec 0 other "\n")

-- NOTE: shutdownHaskellAndExit must be called "safe", because it *can*
-- re-enter Haskell land through finalizers.
foreign import ccall "shutdownHaskellAndExit" 
  shutdownHaskellAndExit :: Int -> IO ()

reportStackOverflow :: Bool -> IO ()
reportStackOverflow bombOut = do
   (hFlush stdout) `catchException` (\ _ -> return ())
   callStackOverflowHook
   if bombOut then
     stg_exit 2
    else
     return ()

reportError :: Bool -> String -> IO ()
reportError bombOut str = do
   (hFlush stdout) `catchException` (\ _ -> return ())
   withCStringLen str $ \(cstr,len) -> do
     writeErrString errorHdrHook cstr len
     if bombOut 
	then stg_exit 1
        else return ()

#ifndef ILX
foreign label "ErrorHdrHook" errorHdrHook :: Ptr ()
#else
foreign import "ErrorHdrHook" errorHdrHook :: Ptr ()
#endif

foreign import ccall "writeErrString__" unsafe
	writeErrString :: Ptr () -> CString -> Int -> IO ()

-- SUP: Are the hooks allowed to re-enter Haskell land?  If so, remove
-- the unsafe below.
foreign import ccall "stackOverflow" unsafe
	callStackOverflowHook :: IO ()

foreign import ccall "stg_exit" unsafe
	stg_exit :: Int -> IO ()
\end{code}

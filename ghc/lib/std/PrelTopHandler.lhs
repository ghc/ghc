-- -----------------------------------------------------------------------------
-- $Id: PrelTopHandler.lhs,v 1.2 2001/05/22 15:06:47 simonmar Exp $
--
-- (c) The University of Glasgow, 2001
--
-- PrelTopHandler
--
-- 'Top-level' IO actions want to catch exceptions (e.g., forkIO and 
-- PrelMain.mainIO) and report them - topHandler is the exception
-- handler they should use for this:

-- make sure we handle errors while reporting the error!
-- (e.g. evaluating the string passed to 'error' might generate
--  another error, etc.)

-- These functions can't go in PrelMain, because PrelMain isn't
-- included in HSstd.o (because PrelMain depends on Main, which
-- doesn't exist yet...).

\begin{code}
module PrelTopHandler (
   topHandler, reportStackOverflow, reportError 
  ) where

import IO

import PrelCString
import PrelPtr
import PrelIOBase
import PrelException

topHandler :: Exception -> IO ()
topHandler err = catchException (real_handler err) topHandler

real_handler :: Exception -> IO ()
real_handler ex =
  case ex of
	AsyncException StackOverflow -> reportStackOverflow True

	-- only the main thread gets ExitException exceptions
	ExitException ExitSuccess     -> shutdownHaskellAndExit 0
	ExitException (ExitFailure n) -> shutdownHaskellAndExit n

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
     writeErrString addrOf_ErrorHdrHook cstr len
     if bombOut 
	then stg_exit 1
        else return ()

foreign import ccall "addrOf_ErrorHdrHook" unsafe
        addrOf_ErrorHdrHook :: Ptr ()

foreign import ccall "writeErrString__" unsafe
	writeErrString :: Ptr () -> CString -> Int -> IO ()

-- SUP: Are the hooks allowed to re-enter Haskell land?  If so, remove
-- the unsafe below.
foreign import ccall "stackOverflow" unsafe
	callStackOverflowHook :: IO ()

foreign import ccall "stg_exit" unsafe
	stg_exit :: Int -> IO ()
\end{code}

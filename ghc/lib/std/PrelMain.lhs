% ------------------------------------------------------------------------------
% $Id: PrelMain.lhs,v 1.8 2001/05/18 16:54:05 simonmar Exp $
%
% (c) The University of Glasgow, 1994-2000
%

\section[PrelMain]{Module @PrelMain@}

\begin{code}
module PrelMain( mainIO, reportStackOverflow, reportError ) where

import Prelude
import {-# SOURCE #-} qualified Main	-- for type of "Main.main"

import IO
import PrelCString
import PrelPtr
import PrelException
\end{code}

\begin{code}
mainIO :: IO ()		-- It must be of type (IO t) because that's what
			-- the RTS expects.  GHC doesn't check this, so
			-- make sure this type signature stays!
mainIO = catchException Main.main topHandler

-- 'Top-level' IO actions want to catch exceptions (e.g., forkIO and 
-- PrelMain.mainIO) and report them - topHandler is the exception
-- handler they should use for this:

-- make sure we handle errors while reporting the error!
-- (e.g. evaluating the string passed to 'error' might generate
--  another error, etc.)
topHandler :: Exception -> IO ()
topHandler err = catchException (real_handler err) topHandler

real_handler :: Exception -> IO ()
real_handler ex =
  case ex of
	AsyncException StackOverflow -> reportStackOverflow True
	ErrorCall s -> reportError True s
	other       -> reportError True (showsPrec 0 other "\n")

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

%
% (c) The AQUA Project, Glasgow University, 1994-1996
%
\section[IOExts]{Module @IOExts@}

@IOExts@ provides useful functionality that fall outside the
standard Haskell IO interface. Expect the contents of IOExts
to be the same for Hugs and GHC (same goes for any other
Hugs/GHC extension libraries, unless a function/type is
explicitly flagged as being implementation specific
extension.)

\begin{code}
{-# OPTIONS -fno-implicit-prelude #-}

module IOExts
        ( fixIO
        , unsafePerformIO
        , unsafeInterleaveIO

        , IORef		    -- instance of: Eq
        , newIORef
        , readIORef
        , writeIORef

	, IOArray	-- instance of: Eq
	, newIOArray
	, boundsIOArray
	, readIOArray
	, writeIOArray
	, freezeIOArray
	
	, openFileEx
	, IOModeEx(..)

        , hSetEcho
	, hGetEcho
	, hIsTerminalDevice
	, hConnectTo

        , trace
        , performGC
	
	, reallyUnsafePtrEq
	, unsafeIOToST

        ) where

\end{code}

\begin{code}
import PrelBase
import PrelIOBase
import PrelHandle ( openFileEx, IOModeEx(..),
		    hSetEcho, hGetEcho, getHandleFd
		  )
import PrelST
import PrelArr
import PrelGHC
import Ix
import IO
import PrelHandle
import PrelErr

reallyUnsafePtrEq :: a -> a -> Bool
reallyUnsafePtrEq a b =
    case reallyUnsafePtrEquality# a b of
	 0# -> False
	 _  -> True
\end{code}

\begin{code}
newtype IORef a = IORef (MutableVar RealWorld a) 
    deriving Eq

newIORef :: a -> IO (IORef a)
newIORef v = stToIO (newVar v) >>= \ var -> return (IORef var)

readIORef :: IORef a -> IO a
readIORef (IORef var) = stToIO (readVar var)

writeIORef :: IORef a -> a -> IO ()
writeIORef (IORef var) v = stToIO (writeVar var v)
\end{code}

\begin{code}
newtype IOArray ix elt = IOArray (MutableArray RealWorld ix elt)
    deriving Eq

newIOArray          :: Ix ix => (ix,ix) -> elt -> IO (IOArray ix elt)
boundsIOArray       :: Ix ix => IOArray ix elt -> (ix, ix)
readIOArray         :: Ix ix => IOArray ix elt -> ix -> IO elt
writeIOArray        :: Ix ix => IOArray ix elt -> ix -> elt -> IO ()
freezeIOArray       :: Ix ix => IOArray ix elt -> IO (Array ix elt)

newIOArray ixs elt = 
    stToIO (newArray ixs elt) >>= \arr -> 
    return (IOArray arr)

boundsIOArray (IOArray arr) = boundsOfArray arr

readIOArray (IOArray arr) ix = stToIO (readArray arr ix)

writeIOArray (IOArray arr) ix elt = stToIO (writeArray arr ix elt)

freezeIOArray (IOArray arr) = stToIO (freezeArray arr)
\end{code}

\begin{code}
{-# NOINLINE trace #-}
trace :: String -> a -> a
trace string expr = unsafePerformIO $ do
    fd <- getHandleFd stderr
    hPutStrLn stderr string
    _ccall_ PostTraceHook fd
    return expr

\end{code}

\begin{code}
unsafeIOToST	   :: IO a -> ST s a
unsafeIOToST (IO io) = ST $ \ s ->
    case ((unsafeCoerce# io) s) of
      IOok   new_s a -> unsafeCoerce# (STret new_s a)
      IOfail new_s e -> error ("I/O Error (unsafeIOToST): " ++ showsPrec 0 e "\n")
\end{code}

Not something you want to call normally, but useful
in the cases where you do want to flush stuff out of
the heap or make sure you've got room enough

\begin{code}
performGC :: IO ()
performGC = _ccall_GC_ StgPerformGarbageCollection
\end{code}

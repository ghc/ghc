%
% (c) The AQUA Project, Glasgow University, 1994-1996
%

\section[IOExts]{Module @IOExts@}

\begin{code}
{-# OPTIONS -fno-implicit-prelude #-}

module IOExts
        ( fixIO
        , unsafePerformIO
        , unsafeInterleaveIO

        , IORef
          -- instance Eq (IORef a)
        , newIORef
        , readIORef
        , writeIORef

	, IOArray
	  -- instance Eq (IOArray ix a)
	, newIOArray
	, boundsIOArray
	, readIOArray
	, writeIOArray
	, freezeIOArray

        , trace
        , performGC
	
	, reallyUnsafePtrEq
        ) where
\end{code}

\begin{code}
import PrelBase
import IOBase
import STBase
import Unsafe
import GHC

reallyUnsafePtrEq a b =
    case reallyUnsafePtrEquality# a b of
	 0# -> False
	 _  -> True

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

boundsIOArray (IOArray arr) = boundsOfArray

readIOArray (IOArray arr) ix = stToIO (readArray arr ix)

writeIOArray (IOArray arr) ix elt = stToIO (writeArray arr ix elt)

freezeIOArray (IOArray arr) = stToIO (freezeArray arr)
\end{code}

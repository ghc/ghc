%
% (c) The AQUA Project, Glasgow University, 1994-1996
%

\section[IORef]{Module @IORef@}

\begin{code}
{-# OPTIONS -fno-implicit-prelude #-}

module IORef (
	IORef,
	newIORef,
	readIORef,
	writeIORef
   ) where

import PrelBase
import ArrBase
import IOBase
import STBase
\end{code}

\begin{code}
newtype IORef a = IORef (MutableVar RealWorld a) deriving Eq

newIORef :: a -> IO (IORef a)
newIORef v = stToIO (newVar v) >>= \ var -> return (IORef var)

readIORef :: IORef a -> IO a
readIORef (IORef var) = stToIO (readVar var)

writeIORef :: IORef a -> a -> IO ()
writeIORef (IORef var) v = stToIO (writeVar var v)
\end{code}

%
% (c) The AQUA Project, Glasgow University, 1994-1996
%
\section[MutVar]{Mutable variables}

Mutable variables, for the @IO@ monad.

\begin{code}
module MutVar

       (
        MutVar,      -- abstract

	newVar,      -- :: a -> IO (MutVar a)
	readVar,     -- :: MutVar a -> IO a
	writeVar,    -- :: MutVar a -> a -> IO ()
	sameVar      -- :: MutVar a -> MutVar a -> Bool

       ) where

import qualified ST
import qualified ArrBase
import IOBase ( IO , stToIO )
import GHC (RealWorld)

\end{code}

\begin{code}

newtype MutVar a = MutVar (ArrBase.MutableVar RealWorld a)

newVar :: a -> IO (MutVar a)
newVar v = stToIO (ST.newVar v) >>= \ var -> return (MutVar var)

readVar :: MutVar a -> IO a
readVar (MutVar var) = stToIO (ST.readVar var)

writeVar :: MutVar a -> a -> IO ()
writeVar (MutVar var) v = stToIO (ST.writeVar var v)

sameVar :: MutVar a -> MutVar a -> Bool
sameVar (MutVar var1) (MutVar var2) = ST.sameVar var1 var2

\end{code}

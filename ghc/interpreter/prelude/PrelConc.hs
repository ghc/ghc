#include "options.h"

#ifndef PROVIDE_CONCURRENT
module PrelConc () where
#else
#ifdef HEAD
module PrelConc (

		-- Thread Ids
	ThreadId,

		-- Forking and suchlike
	forkIO,	
	killThread,
	--par, fork,
	{-threadDelay, threadWaitRead, threadWaitWrite, -}

  		-- MVars
	MVar, newMVar, newEmptyMVar, takeMVar, putMVar, readMVar, swapMVar

    ) where

--infixr 0 `par`, `fork`
import PreludeBuiltin
#endif /* HEAD */
#ifdef BODY

data ThreadId

forkIO :: IO () -> IO ThreadId
forkIO action = primFork (unsafePerformIO action)

killThread :: ThreadId -> IO ()
killThread = primKillThread

data MVar a

instance Eq (MVar a) where (==) = primSameMVar

newEmptyMVar  :: IO (MVar a)
newMVar :: a -> IO (MVar a)
putMVar  :: MVar a -> a -> IO ()
takeMVar :: MVar a -> IO a
readMVar :: MVar a -> IO a
swapMVar :: MVar a -> a -> IO a

newEmptyMVar = primNewMVar
putMVar      = primPutMVar
takeMVar     = primTakeMVar

newMVar value =
    newEmptyMVar	>>= \ mvar ->
    putMVar mvar value	>>
    return mvar

readMVar mvar =
    takeMVar mvar	>>= \ value ->
    putMVar mvar value	>>
    return value

swapMVar mvar new =
    takeMVar mvar	>>= \ old ->
    putMVar mvar new	>>
    return old

#endif /* BODY */

#endif /* PROVIDE_CONCURRENT */


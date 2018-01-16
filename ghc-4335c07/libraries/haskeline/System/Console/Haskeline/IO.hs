{- |
This module provides a stateful, IO-based interface to Haskeline, which may be easier to
integrate into some existing programs or libraries.

It is strongly recommended to use the safer, monadic API of
"System.Console.Haskeline", if possible, rather than the explicit state management
functions of this module.

The equivalent REPL example is:

@
import System.Console.Haskeline
import System.Console.Haskeline.IO
import Control.Concurrent

main = bracketOnError (initializeInput defaultSettings)
            cancelInput -- This will only be called if an exception such
                            -- as a SigINT is received.
            (\\hd -> loop hd >> closeInput hd)
    where
        loop :: InputState -> IO ()
        loop hd = do
            minput <- queryInput hd (getInputLine \"% \")
            case minput of
                Nothing -> return ()
                Just \"quit\" -> return ()
                Just input -> do queryInput hd $ outputStrLn
                                    $ \"Input was: \" ++ input
                                 loop hd
@


-}
module System.Console.Haskeline.IO(
                        InputState(),
                        initializeInput,
                        closeInput,
                        cancelInput,
                        queryInput
                        ) where

import System.Console.Haskeline hiding (completeFilename)
import Control.Concurrent

import Control.Monad.IO.Class

-- Providing a non-monadic API for haskeline
-- A process is forked off which runs the monadic InputT API
-- and actions to be run are passed to it through the following MVars.

data Request = forall a . Request (InputT IO a) (MVar a)

data InputState = HD {forkedThread :: ThreadId,
                        requestVar :: MVar (Maybe Request),
                        subthreadFinished :: MVar ()
                    }

-- | Initialize a session of line-oriented user interaction.
initializeInput :: Settings IO -> IO InputState
initializeInput settings = do
    reqV <- newEmptyMVar
    finished <- newEmptyMVar
    tid <- forkIO (runHaskeline settings reqV finished)
    return HD {requestVar = reqV, forkedThread = tid,
                subthreadFinished = finished}

runHaskeline :: Settings IO -> MVar (Maybe Request) -> MVar () -> IO ()
runHaskeline settings reqV finished = runInputT settings loop
                    `finally` putMVar finished ()
    where
        loop = do
            mf <- liftIO $ takeMVar reqV
            case mf of
                Nothing -> return ()
                Just (Request f var) -> f >>= liftIO . putMVar var >> loop

-- | Finish and clean up the line-oriented user interaction session.  Blocks on an
-- existing call to 'queryInput'.
closeInput :: InputState -> IO ()
closeInput hd = putMVar (requestVar hd) Nothing >> takeMVar (subthreadFinished hd)

-- | Cancel and clean up the user interaction session.  Does not block on an existing
-- call to 'queryInput'.
cancelInput :: InputState -> IO ()
cancelInput hd = killThread (forkedThread hd) >> takeMVar (subthreadFinished hd)

-- | Run one action (for example, 'getInputLine') as part of a session of user interaction.
--
-- For example, multiple calls to 'queryInput' using the same 'InputState' will share
-- the same input history.  In constrast, multiple calls to 'runInputT' will use distinct
-- histories unless they share the same history file.
--
-- This function should not be called on a closed or cancelled 'InputState'.
queryInput :: InputState -> InputT IO a -> IO a
queryInput hd f = do
    var <- newEmptyMVar
    putMVar (requestVar hd) (Just (Request f var))
    takeMVar var



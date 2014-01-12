module T7702Plugin ( plugin )  where

import GhcPlugins

-- A plugin that does nothing but tickle CoreM's writer.
plugin :: Plugin
plugin = defaultPlugin { installCoreToDos = install }
    where
        install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
        install _ todos = do
            reinitializeGlobals

            putMsgS "T7702Plugin"

            -- 1 million times, so the allocation in this plugin dominates allocation due
            -- to other compiler flags and the test framework can easily catch the difference
            -- can't use replicateM_ because it causes its own problems
            nothingX100000 ; nothingX100000 ; nothingX100000 ; nothingX100000 ; nothingX100000
            nothingX100000 ; nothingX100000 ; nothingX100000 ; nothingX100000 ; nothingX100000

            return todos

-- this will result in a call to plusWriter in CoreM's
-- >>= implementation, which was causing the space leak
nothing :: CoreM ()
nothing = liftIO (return ())

nothingX10 :: CoreM ()
nothingX10 = do
    nothing ; nothing ; nothing ; nothing ; nothing
    nothing ; nothing ; nothing ; nothing ; nothing

nothingX100 :: CoreM ()
nothingX100 = do
    nothingX10 ; nothingX10 ; nothingX10 ; nothingX10 ; nothingX10
    nothingX10 ; nothingX10 ; nothingX10 ; nothingX10 ; nothingX10

nothingX1000 :: CoreM ()
nothingX1000 = do
    nothingX100 ; nothingX100 ; nothingX100 ; nothingX100 ; nothingX100
    nothingX100 ; nothingX100 ; nothingX100 ; nothingX100 ; nothingX100

nothingX10000 :: CoreM ()
nothingX10000 = do
    nothingX1000 ; nothingX1000 ; nothingX1000 ; nothingX1000 ; nothingX1000
    nothingX1000 ; nothingX1000 ; nothingX1000 ; nothingX1000 ; nothingX1000

nothingX100000 :: CoreM ()
nothingX100000 = do
    nothingX10000 ; nothingX10000 ; nothingX10000 ; nothingX10000 ; nothingX10000
    nothingX10000 ; nothingX10000 ; nothingX10000 ; nothingX10000 ; nothingX10000

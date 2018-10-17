module Common where

import GhcPlugins

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install options todos = do
    putMsgS $ "Simple Plugin Passes Queried"
    putMsgS $ "Got options: " ++ unwords options

    -- Create some actual passes to continue the test.
    return $ CoreDoPluginPass "Main pass" mainPass
             : todos

mainPass :: ModGuts -> CoreM ModGuts
mainPass guts = do
    putMsgS "Simple Plugin Pass Run"
    return guts

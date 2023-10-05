{-# LANGUAGE CPP #-}
module Common where

import GHC.Plugins

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install options todos = do
    putMsgS $ "Simple Plugin Passes Queried"
    putMsgS $ "Got options: " ++ unwords options

    -- Create some actual passes to continue the test.
    return $ CoreDoPluginPass "Main pass" mainPass
             : todos

mainPass :: ModGuts -> CoreM ModGuts
mainPass guts = do
#if defined(RUN2)
    putMsgS "Simple Plugin Pass Run 2"
#else
    putMsgS "Simple Plugin Pass Run"
#endif
    return guts

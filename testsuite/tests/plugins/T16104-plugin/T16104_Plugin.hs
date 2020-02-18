{-# LANGUAGE TemplateHaskell #-}

module T16104_Plugin (plugin) where

import GHC.Plugins
import Data.Bits

plugin :: Plugin
plugin = defaultPlugin {installCoreToDos = install}
  where install _ todos = return (test : todos)

        test = CoreDoPluginPass "Test" check

        check :: ModGuts -> CoreM ModGuts
        check m = do mbN <- thNameToGhcName 'complement
                     case mbN of
                       Just _  -> liftIO $ putStrLn "Found complement!"
                       Nothing -> error "Failed to locate complement"

                     return m

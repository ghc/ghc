module CHSC (Supercompile(..), plugin) where

import Supercompile
import GhcPlugins

import Data.Data     (Data)
import Data.Typeable (Typeable)
import Data.List     (nub)


-- The supercomplier behaves as follows:
--  1. If the command line contains -fplugin-opt=CHSC:supercompile or the module is annotated
--     with Supercompile then we supercompile the whole module
--  2. Otherwise, we supercompile any individual definitions annoted with Supercompile

data Supercompile = Supercompile deriving (Data, Typeable)


plugin :: Plugin
plugin = defaultPlugin {
    installCoreToDos = install
  }

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install options todos = do
    unconditional <- case nub options of
        []               -> return False
        ["supercompile"] -> return True
        _                -> fail "CHSC: the only recognised command line option is -fplugin-opt=CHSC:supercompile"
    return $ CoreDoPluginPass "Supercompile (CHSC)" (pass unconditional) : todos

pass :: Bool -> ModGuts -> CoreM ModGuts
pass unconditional guts = do
    -- Determine which top-level binders should be supercompiled
    should_sc <- case unconditional of
        True  -> return (const True)
        False -> do
            anns :: UniqFM Supercompile <- getFirstAnnotations deserializeWithData guts
            mod <- getModule
            return $ if mod `elemUFM` anns
                      then const True
                      else (`elemUFM` anns)
    -- Do the deed
    bindsOnlyPass (return . supercompileProgramSelective should_sc) guts

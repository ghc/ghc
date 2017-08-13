module Rules.Configure (configureRules) where

import qualified System.Info as System

import Base
import CmdLineFlag
import Context
import GHC
import Target
import UserSettings
import Utilities

configureRules :: Rules ()
configureRules = do
    [configFile, "settings", configH] &%> \outs -> do
        if cmdSkipConfigure
        then unlessM (doesFileExist configFile) $
            error $ "Configuration file " ++ configFile ++ " is missing."
                ++ "\nRun the configure script manually or do not use the "
                ++ "--skip-configure flag."
        else do
            -- We cannot use windowsHost here due to a cyclic dependency.
            when (System.os == "mingw32") $ do
                putBuild "| Checking for Windows tarballs..."
                quietly $ cmd ["bash", "mk/get-win32-tarballs.sh", "download", System.arch]
            let srcs    = map (<.> "in") outs
                context = vanillaContext Stage0 compiler
            need srcs
            build $ target context (Configure ".") srcs outs

    ["configure", configH <.> "in"] &%> \_ -> do
        if cmdSkipConfigure
        then unlessM (doesFileExist "configure") $
            error $ "The configure script is missing.\nRun the boot script"
                ++ " manually or do not use the --skip-configure flag."
        else do
            need ["configure.ac"]
            putBuild "| Running boot..."
            quietly $ cmd "perl boot"

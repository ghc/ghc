module Rules.Configure (configureRules) where

import qualified System.Info

import Base
import Builder
import CmdLineFlag (cmdSkipConfigure)
import Context
import GHC (compiler)
import Rules.Actions
import Rules.Generators.GhcAutoconfH
import Stage
import Target

configureRules :: Rules ()
configureRules = do
    [configFile, "settings", configH] &%> \outs -> do
        if cmdSkipConfigure
        then unlessM (doesFileExist configFile) $
            putError $ "Configuration file " ++ configFile ++ " is missing."
                ++ "\nRun the configure script manually or do not use the "
                ++ "--skip-configure flag."
        else do
            -- We cannot use windowsHost here due to a cyclic dependency.
            when (System.Info.os == "mingw32") $ do
                putBuild "| Checking for Windows tarballs..."
                quietly $ cmd [ "bash"
                              , "mk/get-win32-tarballs.sh"
                              , "download"
                              , System.Info.arch ]
            let srcs    = map (<.> "in") outs
                context = vanillaContext Stage0 compiler
            need srcs
            build $ Target context (Configure ".") srcs outs

    ["configure", configH <.> "in"] &%> \_ -> do
        if cmdSkipConfigure
        then unlessM (doesFileExist "configure") $
            putError $ "The configure script is missing.\nRun the boot script"
                ++ " manually or do not use the --skip-configure flag."
        else do
            need ["configure.ac"]
            putBuild "| Running boot..."
            quietly $ cmd (EchoStdout False) "perl boot"

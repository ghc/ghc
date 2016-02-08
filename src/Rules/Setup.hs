module Rules.Setup (setupRules) where

import qualified System.Info

import Base
import CmdLineFlag
import Rules.Actions
import Rules.Generators.GhcAutoconfH

setupRules :: Rules ()
setupRules = do
    -- We always rerun the configure script in this mode, because the flags
    -- passed to it can affect the contents of system.config file.
    [configFile, "settings", configH] &%> \[cfg, settings, cfgH] -> do
        alwaysRerun
        case cmdConfigure of
            RunConfigure args -> do
                need [ settings <.> "in", cfgH <.> "in" ]
                -- We cannot use windowsHost here due to a cyclic dependency
                let defaultArgs = if System.Info.os == "mingw32"
                                  then [ "--enable-tarballs-autodownload" ]
                                  else []
                runConfigure "." [] $ defaultArgs ++ [args]
            SkipConfigure     -> unlessM (doesFileExist cfg) $
                putError $ "Configuration file " ++ cfg ++ " is missing.\n"
                    ++ "Run the configure script either manually or via the "
                    ++ "build system by passing --configure[=ARGS] flag."

    ["configure", configH <.> "in"] &%> \_ -> do
        putBuild "| Running boot..."
        quietly $ cmd (EchoStdout False) "perl boot"

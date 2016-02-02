module Rules.Config (configRules) where

import Base
import CmdLineFlag
import Rules.Actions
import Rules.Generators.GhcAutoconfH

configRules :: Rules ()
configRules = do
    -- We always rerun the configure script in this mode, because the flags
    -- passed to it can affect the contents of system.config file.
    [configFile, "settings", configH] &%> \[cfg, settings, cfgH] -> do
        alwaysRerun
        case cmdConfigure of
            RunConfigure args -> do
                need [ settings <.> "in", cfgH <.> "in" ]
                runConfigure "." [] [args]
            SkipConfigure     -> unlessM (doesFileExist cfg) $
                putError $ "Configuration file " ++ cfg ++ " is missing.\n"
                    ++ "Run the configure script either manually or via the "
                    ++ "build system by passing --configure[=ARGS] flag."

    ["configure", configH <.> "in"] &%> \_ -> do
        putBuild "| Running boot..."
        quietly $ cmd (EchoStdout False) "perl boot"

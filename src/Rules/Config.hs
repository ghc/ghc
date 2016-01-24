module Rules.Config (configRules) where

import Base
import CmdLineFlag
import Rules.Actions

configRules :: Rules ()
configRules = do
    -- We always rerun the configure script in this mode, because the flags
    -- passed to it can affect the contents of system.config file.
    configPath -/- "system.config" %> \out -> do
        alwaysRerun
        case cmdConfigure of
            RunConfigure args -> runConfigure "." [] [args]
            SkipConfigure     -> unlessM (doesFileExist out) $
                putError $ "Configuration file " ++ out ++ " is missing.\n"
                    ++ "Run the configure script either manually or via the "
                    ++ "build system by passing --configure[=ARGS] flag."

    -- When we detect Windows paths in ACLOCAL_PATH we reset it.
    -- TODO: Handle Windows paths in ACLOCAL_PATH more gracefully.
    "configure" %> \_ -> do
        putBuild "| Running boot..."
        quietly $ cmd (EchoStdout False) "perl boot"

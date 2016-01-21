module Rules.Config (configRules) where

import Base
import CmdLineFlag
import Rules.Actions

configRules :: Rules ()
configRules = case cmdConfigure of
    SkipConfigure     -> mempty
    RunConfigure args -> do
        configPath -/- "system.config" %> \_ -> do
            need [configPath -/- "system.config.in"]
            runConfigure "." [] [args]

        "configure" %> \_ -> do
            putBuild "| Running boot..."
            unit $ cmd "perl boot"

module Rules.Config (configRules) where

import Base
import Settings.User

-- TODO: Consider removing this file.
configRules :: Rules ()
configRules = when buildSystemConfigFile $ do
    configPath -/- "system.config" %> \_ -> do
        need [configPath -/- "system.config.in", "configure"]
        putBuild "Running configure..."
        cmd "bash configure" -- TODO: get rid of 'bash'

    "configure" %> \_ -> do
        putBuild "Running autoconf..."
        cmd "bash autoconf" -- TODO: get rid of 'bash'

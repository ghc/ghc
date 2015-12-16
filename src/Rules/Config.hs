module Rules.Config (configRules) where

import Base

configRules :: Rules ()
configRules = do
    configPath -/- "system.config" %> \_ -> do
        need [configPath -/- "system.config.in", "configure"]
        putBuild "Running configure..."
        cmd "bash configure" -- TODO: get rid of 'bash'

    "configure" %> \_ -> do
        putBuild "Running autoconf..."
        cmd "bash autoconf" -- TODO: get rid of 'bash'

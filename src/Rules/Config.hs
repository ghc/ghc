module Rules.Config (configRules) where

import Base
import Util

configRules :: Rules ()
configRules = do
    configPath -/- "system.config" %> \out -> do
        need [configPath -/- "system.config.in", "configure"]
        putBuild "Running configure..."
        cmd "bash configure" -- TODO: get rid of 'bash'

    "configure" %> \out -> do
        copyFile' (configPath -/- "configure.ac") "configure.ac"
        putBuild "Running autoconf..."
        cmd "bash autoconf" -- TODO: get rid of 'bash'

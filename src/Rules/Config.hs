module Rules.Config (
    autoconfRules, configureRules
    ) where

import Util
import Oracles.Base

autoconfRules :: Rules ()
autoconfRules = do
    "configure" %> \out -> do
        copyFile' (configPath </> "configure.ac") "configure.ac"
        putColoured White $ "Running autoconf..."
        cmd "bash autoconf" -- TODO: get rid of 'bash'

configureRules :: Rules ()
configureRules = do
    configPath </> "system.config" %> \out -> do
        need [configPath </> "system.config.in", "configure"]
        putColoured White "Running configure..."
        cmd "bash configure" -- TODO: get rid of 'bash'

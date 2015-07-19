module Config (
    autoconfRules, configureRules, cfgPath
    ) where

import Util
import Development.Shake
import Development.Shake.FilePath

cfgPath :: FilePath
cfgPath = "shake" </> "cfg"

autoconfRules :: Rules ()
autoconfRules = do
    "configure" %> \out -> do
        copyFile' (cfgPath </> "configure.ac") "configure.ac"
        putColoured White $ "Running autoconf..."
        cmd "bash autoconf" -- TODO: get rid of 'bash'

configureRules :: Rules ()
configureRules = do
    cfgPath </> "system.config" %> \out -> do
        need [cfgPath </> "system.config.in", "configure"]
        putColoured White "Running configure..."
        cmd "bash configure" -- TODO: get rid of 'bash'

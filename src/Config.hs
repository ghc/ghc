module Config (
    autoconfRules, configureRules, cfgPath
    ) where

import Base
import Util

cfgPath :: FilePath
cfgPath = "shake" </> "cfg"

autoconfRules :: Rules ()
autoconfRules = do
    "configure" %> \out -> do
        need ["shake/src/Config.hs"]
        copyFile' (cfgPath </> "configure.ac") "configure.ac"
        putColoured Vivid White $ "Running autoconf..."
        cmd "bash autoconf" -- TODO: get rid of 'bash'

configureRules :: Rules ()
configureRules = do
    cfgPath </> "default.config" %> \out -> do
        need ["shake/src/Config.hs"]
        need [cfgPath </> "default.config.in", "configure"]
        putColoured Vivid White "Running configure..."
        cmd "bash configure" -- TODO: get rid of 'bash'

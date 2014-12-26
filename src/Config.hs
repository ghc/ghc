module Config (
    autoconfRules, configureRules, cfgPath
    ) where

import Base

cfgPath :: FilePath
cfgPath = "shake" </> "cfg"

autoconfRules :: Rules ()
autoconfRules = do
    "configure" %> \out -> do
        copyFile' (cfgPath </> "configure.ac") "configure.ac"
        cmd "bash autoconf" -- TODO: get rid of 'bash'

configureRules :: Rules ()
configureRules = do
    cfgPath </> "default.config" %> \out -> do
        need [cfgPath </> "default.config.in", "configure"]
        cmd "bash configure" -- TODO: get rid of 'bash'

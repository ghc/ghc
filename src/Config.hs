module Config (
    autoconfRules, configureRules, cfgPath
    ) where

import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Rule
import Control.Applicative
import Control.Monad
import Base

cfgPath :: FilePath
cfgPath = "shake" </> "cfg"

autoconfRules :: Rules ()
autoconfRules = do
    "configure" %> \out -> do
        copyFile' (cfgPath </> "configure.ac") "configure.ac"
        cmd "bash autoconf"

configureRules :: Rules ()
configureRules = do
    cfgPath </> "default.config" %> \out -> do
        need [cfgPath </> "default.config.in", "configure"]
        cmd "bash configure"

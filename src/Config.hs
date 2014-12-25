module Config (
    autoconfRules, configureRules
    ) where

import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Rule
import Control.Applicative
import Control.Monad
import Base
import Oracles

autoconfRules :: Rules ()
autoconfRules = do
    "shake/configure" %> \out -> do
        need ["shake/configure.ac"]
        cmd $ "bash shake/autoconf"

configureRules :: Rules ()
configureRules = do
    "shake/default.config" %> \out -> do
        need ["shake/default.config.in", "shake/configure"]
        cmd $ "bash shake/configure"

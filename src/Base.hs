module Base (
    -- * General utilities
    module Control.Applicative,
    module Control.Monad.Extra,
    module Data.List.Extra,
    module Data.Maybe,
    module Data.Semigroup,
    module Hadrian.Utilities,

    -- * Shake
    module Development.Shake,
    module Development.Shake.Classes,
    module Development.Shake.FilePath,
    module Development.Shake.Util,

    -- * Basic data types
    module Builder,
    module Package,
    module Stage,
    module Way,

    -- * Paths
    configPath, configFile, sourcePath, configH
    ) where

import Control.Applicative
import Control.Monad.Extra
import Control.Monad.Reader
import Data.List.Extra
import Data.Maybe
import Data.Semigroup
import Development.Shake hiding (parallel, unit, (*>), Normal)
import Development.Shake.Classes
import Development.Shake.FilePath
import Development.Shake.Util
import Hadrian.Utilities

import Builder
import Package
import Stage
import Way

-- | Hadrian lives in 'hadrianPath' directory of the GHC tree.
hadrianPath :: FilePath
hadrianPath = "hadrian"

-- TODO: Move this to build directory?
configPath :: FilePath
configPath = hadrianPath -/- "cfg"

-- | Path to the file with configuration settings.
configFile :: FilePath
configFile = configPath -/- "system.config"

-- | Path to source files of the build system, e.g. this file is located at
-- sourcePath -/- "Base.hs". We use this to `need` some of the source files.
sourcePath :: FilePath
sourcePath = hadrianPath -/- "src"

-- TODO: change @mk/config.h@ to @shake-build/cfg/config.h@
-- | Path to the generated @mk/config.h file.
configH :: FilePath
configH = "mk/config.h"

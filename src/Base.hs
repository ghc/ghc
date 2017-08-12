module Base (
    -- * General utilities
    module Control.Applicative,
    module Control.Monad.Extra,
    module Data.Bifunctor,
    module Data.Function,
    module Data.List.Extra,
    module Data.Maybe,
    module Data.Semigroup,

    -- * Shake
    module Development.Shake,
    module Development.Shake.Classes,
    module Development.Shake.FilePath,

    -- * Paths
    configPath, configFile, sourcePath,

    -- * Miscellaneous utilities
    unifyPath, quote, (-/-)
    ) where

import Control.Applicative
import Control.Monad.Extra
import Control.Monad.Reader
import Data.Bifunctor
import Data.Function
import Data.List.Extra
import Data.Maybe
import Data.Semigroup
import Development.Shake hiding (parallel, unit, (*>), Normal)
import Development.Shake.Classes
import Development.Shake.FilePath
import Hadrian.Utilities

-- TODO: reexport Stage, etc.?

-- | Hadrian lives in 'hadrianPath' directory of the GHC tree.
hadrianPath :: FilePath
hadrianPath = "hadrian"

-- TODO: Move this to build directory?
configPath :: FilePath
configPath = hadrianPath -/- "cfg"

configFile :: FilePath
configFile = configPath -/- "system.config"

-- | Path to source files of the build system, e.g. this file is located at
-- sourcePath -/- "Base.hs". We use this to `need` some of the source files.
sourcePath :: FilePath
sourcePath = hadrianPath -/- "src"

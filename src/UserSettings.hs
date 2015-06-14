module UserSettings (
    userSettings, userPackages, userWays
    ) where

import Base hiding (arg, args, Args)
import Oracles.Builder
import Ways
import Targets
import Expression
import Expression.Settings

-- No user-specific settings by default
userSettings :: Settings
userSettings = mempty

userPackages :: Packages
userPackages = mempty

userWays :: Ways
userWays = mempty

-- Examples:
userSettings' :: Settings
userSettings' = mconcat
    [ package compiler ? stage Stage0 ? append ["foo", "bar"]
    , builder (Ghc Stage0) ? remove ["-O2"]
    , builder GhcCabal ? removeSub "--configure-option=CFLAGS" ["-Werror"] ]

userPackages' :: Packages
userPackages' = mconcat
    [ stage Stage1 ? remove [cabal]
    ,                remove [compiler] ]

userWays' :: Ways
userWays' = remove [profiling]

module UserSettings (
    userSettings, userPackages, userWays,

    buildHaddock, validating
    ) where

import Base hiding (arg, args, Args)
import Oracles.Builder
import Ways
import Targets
import Switches
import Expression

-- No user-specific settings by default
userSettings :: Settings
userSettings = mempty

userPackages :: Packages
userPackages = mempty

userWays :: Ways
userWays = mempty

-- User-defined predicates
buildHaddock :: Predicate
buildHaddock = return True

validating :: Predicate
validating = return False

-- Examples:
userSettings' :: Settings
userSettings' = mconcat
    [ package compiler     ? stage0 ? append ["foo", "bar"]
    , builder (Ghc Stage0) ? remove ["-O2"]
    , builder GhcCabal     ? removeSub "--configure-option=CFLAGS" ["-Werror"] ]

userPackages' :: Packages
userPackages' = mconcat
    [ stage1 ? remove [cabal]
    ,          remove [compiler] ]

userWays' :: Ways
userWays' = remove [profiling]

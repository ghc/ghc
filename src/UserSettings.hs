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
import Settings.Util

-- No user-specific settings by default
userSettings :: Settings
userSettings = mempty

-- Control conditions of which packages get to be built
-- TODO: adding *new* packages is not possible (see knownPackages in Targets.hs)
userPackages :: Packages
userPackages = mempty

-- Control which ways are built
userWays :: Ways
userWays = mempty

-- User-defined predicates
-- TODO: migrate more predicates here from configuration files
buildHaddock :: Predicate
buildHaddock = return True

validating :: Predicate
validating = return False

-- Examples:
userSettings' :: Settings
userSettings' = mconcat
    [ package base           ?
      builder GhcCabal       ? arg ("--flags=" ++ integerLibraryName)

    , package integerLibrary ? appendCcArgs ["-Ilibraries/integer-gmp2/gmp"]

    , windowsHost            ?
      package integerLibrary ?
      builder GhcCabal       ? arg "--configure-option=--with-intree-gmp"

    , package compiler       ?
      stage0                 ?
      way profiling          ?
      file "pattern.*"       ? args ["foo", "bar"]

    , builder (Ghc Stage0)   ? remove ["-O2"]

    , builder GhcCabal       ? removeSub "--configure-option=CFLAGS" ["-Werror"]
    ]

userPackages' :: Packages
userPackages' = mconcat
    [ stage1 ? remove [cabal]
    ,          remove [compiler] ]

userWays' :: Ways
userWays' = remove [profiling]

module UserSettings (
    userSettings, userPackages
    ) where

import Base hiding (arg, args, Args)
import Rules.Data
import Oracles.Builder
import Expression
import Expression.Settings

userSettings :: Settings
userSettings = mconcat
    [ package compiler ? stage Stage0 ? append ["foo", "bar"]
    , builder (Ghc Stage0) ? remove ["-O2"]
    , builder GhcCabal ? removeSub "--configure-option=CFLAGS" ["-Werror"] ]

userPackages :: Settings
userPackages = mconcat
    [ stage Stage1 ? remove [cabal]
    ,                remove [compiler] ]

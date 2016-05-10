module Settings.Builders.Make (makeBuilderArgs) where

import Expression
import Predicates (builder)
import Settings

makeBuilderArgs :: Args
makeBuilderArgs = mconcat
    [ builder (Make "testsuite/tests") ? arg "fast"
    , builder (Make gmpBuildPath     ) ? arg "MAKEFLAGS="
    , builder (Make libffiBuildPath  ) ? append ["MAKEFLAGS=", "install"] ]

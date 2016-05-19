module Settings.Builders.Make (makeBuilderArgs) where

import Predicate
import Settings

makeBuilderArgs :: Args
makeBuilderArgs = mconcat
    [ builder (Make gmpBuildPath     ) ? arg "MAKEFLAGS="
    , builder (Make libffiBuildPath  ) ? append ["MAKEFLAGS=", "install"]
    , builder (Make "testsuite/tests") ? arg "fast" ]

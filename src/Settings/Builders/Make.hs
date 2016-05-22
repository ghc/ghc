module Settings.Builders.Make (makeBuilderArgs) where

import Base
import Predicate
import Settings

makeBuilderArgs :: Args
makeBuilderArgs = do
    threads <- shakeThreads <$> lift getShakeOptions
    let j = "-j" ++ show threads
    mconcat
        [ builder (Make gmpBuildPath     ) ? append ["MAKEFLAGS=" ++ j]
        , builder (Make libffiBuildPath  ) ? append ["MAKEFLAGS=" ++ j, "install"]
        , builder (Make "testsuite/tests") ? arg "fast" ]

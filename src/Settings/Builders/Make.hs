module Settings.Builders.Make (makeBuilderArgs) where

import Base
import Predicate
import Settings

makeBuilderArgs :: Args
makeBuilderArgs = do
    threads <- shakeThreads <$> lift getShakeOptions
    let t = show threads
    mconcat
        [ builder (Make gmpBuildPath     ) ? append ["MAKEFLAGS=-j" ++ t]
        , builder (Make libffiBuildPath  ) ? append ["MAKEFLAGS=-j" ++ t, "install"]
        , builder (Make "testsuite/tests") ? append ["THREADS=" ++ t, "fast"] ]

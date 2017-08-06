module Settings.Builders.Make (makeBuilderArgs) where

import Settings.Builders.Common

makeBuilderArgs :: Args
makeBuilderArgs = do
    threads <- shakeThreads <$> (expr getShakeOptions)
    let t = show $ max 4 (threads - 2) -- Don't use all Shake's threads
    mconcat
        [ builder (Make gmpBuildPath     ) ? pure ["MAKEFLAGS=-j" ++ t]
        , builder (Make libffiBuildPath  ) ? pure ["MAKEFLAGS=-j" ++ t, "install"]
        , builder (Make "testsuite/tests") ? pure ["THREADS=" ++ t, "fast"] ]

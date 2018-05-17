module Settings.Builders.Make (makeBuilderArgs) where

import Builder
import Rules.Gmp
import Rules.Libffi
import Settings.Builders.Common

makeBuilderArgs :: Args
makeBuilderArgs = do
    threads    <- shakeThreads <$> expr getShakeOptions
    gmpPath    <- expr gmpBuildPath
    libffiPath <- expr libffiBuildPath
    ghcPath    <- expr $
      (-/-) <$> topDirectory <*> builderPath (Ghc CompileHs Stage2)
    perlPath   <- expr $ builderPath Perl
    let t = show $ max 4 (threads - 2) -- Don't use all Shake's threads
    mconcat
        [ builder (Make gmpPath          ) ? pure ["MAKEFLAGS=-j" ++ t]
        , builder (Make libffiPath       ) ? pure ["MAKEFLAGS=-j" ++ t, "install"]
        , builder (Make "testsuite/tests") ? pure ["THREADS=" ++ t, "fast"]
        , builder (Make "nofib"          ) ? pure
            [ "WithNofibHc=" ++ ghcPath
            , "PERL=" ++ perlPath
            ]
        ]

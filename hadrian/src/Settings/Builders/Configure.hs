module Settings.Builders.Configure (configureBuilderArgs) where

import Packages
import Rules.Gmp
import Settings.Builders.Common

configureBuilderArgs :: Args
configureBuilderArgs = do
    stage      <- getStage
    gmpPath    <- expr (gmpBuildPath stage)
    libffiPath <- expr (libffiBuildPath stage)
    mconcat [ builder (Configure gmpPath) ? do
                targetPlatform <- getSetting TargetPlatform
                buildPlatform <- getSetting BuildPlatform
                pure [ "--enable-shared=no"
                     , "--with-pic=yes"
                     , "--host=" ++ targetPlatform    -- GMP's host is our target
                     , "--build=" ++ buildPlatform ]

            , builder (Configure libffiPath) ? do
                top            <- expr topDirectory
                targetPlatform <- getSetting TargetPlatform
                way            <- getWay
                pure [ "--prefix=" ++ top -/- libffiPath -/- "inst"
                     , "--libdir=" ++ top -/- libffiPath -/- "inst/lib"
                     , "--enable-static=yes"
                     , "--enable-shared="
                            ++ (if wayUnit Dynamic way
                                    then "yes"
                                    else "no")
                     , "--host=" ++ targetPlatform ] ]

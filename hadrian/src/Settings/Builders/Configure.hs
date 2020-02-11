module Settings.Builders.Configure (configureBuilderArgs) where

import Packages
import Settings.Builders.Common

configureBuilderArgs :: Args
configureBuilderArgs = do
    root       <- getBuildRoot
    stage      <- getStage
    let gmpPath = root -/- stageString stage -/- "gmp"
    libffiPath <- expr (libffiBuildPath stage)
    mconcat [ builder (Configure gmpPath) ? do
                hostPlatform  <- getSetting HostPlatform
                buildPlatform <- getSetting BuildPlatform
                pure [ "--enable-shared=no"
                     , "--with-pic=yes"
                     , "--host=" ++ hostPlatform
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

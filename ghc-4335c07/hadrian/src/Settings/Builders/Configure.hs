module Settings.Builders.Configure (configureBuilderArgs) where

import Rules.Gmp
import Rules.Libffi
import Settings.Builders.Common

configureBuilderArgs :: Args
configureBuilderArgs = do
    gmpPath    <- expr gmpBuildPath
    libffiPath <- expr libffiBuildPath
    mconcat [ builder (Configure gmpPath) ? do
                hostPlatform  <- getSetting HostPlatform
                buildPlatform <- getSetting BuildPlatform
                pure [ "--enable-shared=no"
                     , "--host=" ++ hostPlatform
                     , "--build=" ++ buildPlatform ]

            , builder (Configure libffiPath) ? do
                top            <- expr topDirectory
                targetPlatform <- getSetting TargetPlatform
                pure [ "--prefix=" ++ top -/- libffiPath -/- "inst"
                     , "--libdir=" ++ top -/- libffiPath -/- "inst/lib"
                     , "--enable-static=yes"
                     , "--enable-shared=no" -- TODO: add support for yes
                     , "--host=" ++ targetPlatform ] ]

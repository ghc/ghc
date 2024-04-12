module Settings.Builders.Configure (configureBuilderArgs) where

import Rules.Gmp
import Settings.Builders.Common
import GHC.Toolchain.Target (targetPlatformTriple)

configureBuilderArgs :: Args
configureBuilderArgs = do
    stage      <- getStage
    gmpPath    <- expr (gmpBuildPath stage)
    libffiPath <- expr (libffiBuildPath stage)
    mconcat [ builder (Configure gmpPath) ? do
                targetArch <- queryTarget queryArch
                targetPlatform <- queryTarget targetPlatformTriple
                buildPlatform <- queryBuild targetPlatformTriple
                pure $ [ "--enable-shared=no"
                     , "--host=" ++ targetPlatform    -- GMP's host is our target
                     , "--build=" ++ buildPlatform ]
                     -- Disable FFT logic on wasm32, sacrifice
                     -- performance of multiplying very large operands
                     -- to save code size
                     <> [ "--disable-fft" | targetArch == "wasm32" ]
                     -- Disable GMP's alloca usage on wasm32, it may
                     -- cause stack overflow (#22602) due to the
                     -- rather small 64KB default stack size. See
                     -- https://gmplib.org/manual/Build-Options for
                     -- more detailed explanation of this configure
                     -- option.
                     <> [ "--enable-alloca=malloc-notreentrant" | targetArch == "wasm32" ]
                     -- Enable PIC unless target is wasm32, in which
                     -- case we don't want libgmp.a to be bloated due
                     -- to PIC overhead.
                     <> [ "--with-pic=yes" | targetArch /= "wasm32" ]

            , builder (Configure libffiPath) ? do
                top            <- expr topDirectory
                targetPlatform <- queryTarget targetPlatformTriple
                way            <- getWay
                pure [ "--prefix=" ++ top -/- libffiPath -/- "inst"
                     , "--libdir=" ++ top -/- libffiPath -/- "inst/lib"
                     , "--enable-static=yes"
                     , "--enable-shared="
                            ++ (if wayUnit Dynamic way
                                    then "yes"
                                    else "no")
                     , "--host=" ++ targetPlatform ] ]

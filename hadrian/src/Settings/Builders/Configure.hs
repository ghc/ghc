module Settings.Builders.Configure (configureBuilderArgs) where

import Rules.Gmp
import Settings.Builders.Common
import GHC.Toolchain.Target (targetPlatformTriple)

configureBuilderArgs :: Args
configureBuilderArgs = do
    stage      <- getStage
    gmpPath    <- expr (gmpBuildPath stage)
    builder (Configure gmpPath) ? do
                targetArch <- queryTarget stage queryArch
                targetPlatform <- queryTarget stage targetPlatformTriple
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
                     <> [ "--with-pic=yes" ]

module Settings.Builders.Configure (configureBuilderArgs) where

import qualified System.Info as System

import Base
import Oracles.Config.Setting
import Oracles.WindowsPath
import Predicate
import Settings.Paths

configureBuilderArgs :: Args
configureBuilderArgs = mconcat
    [ builder (Configure gmpBuildPath) ? do
        hostPlatform  <- getSetting HostPlatform
        buildPlatform <- getSetting BuildPlatform
        append [ "--enable-shared=no"
               , "--host=" ++ hostPlatform
               , "--build=" ++ buildPlatform ]

    , builder (Configure libffiBuildPath) ? do
        top            <- getTopDirectory
        targetPlatform <- getSetting TargetPlatform
        append [ "--prefix=" ++ top -/- libffiBuildPath -/- "inst"
               , "--libdir=" ++ top -/- libffiBuildPath -/- "inst/lib"
               , "--enable-static=yes"
               , "--enable-shared=no" -- TODO: add support for yes
               , "--host=" ++ targetPlatform ]

    -- On OS X, use "nm-classic" instead of "nm" due to a bug in the latter.
    -- See https://ghc.haskell.org/trac/ghc/ticket/11744
    , builder (Configure ".") ? System.os == "darwin" ?
        arg "--with-nm=$(xcrun --find nm-classic)" ]

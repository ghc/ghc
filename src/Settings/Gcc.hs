module Settings.Gcc (gccArgs, includeGccArgs) where

import Base
import Util
import Builder
import Expression
import Oracles.PackageData
import Settings.Util

gccArgs :: Args
gccArgs = stagedBuilder Gcc ? do
    path   <- getTargetPath
    file   <- getFile
    deps   <- getDependencies
    ccArgs <- getPkgDataList CcArgs
    mconcat [ append ccArgs
            , includeGccArgs
            , arg "-c"
            , append $ filter ("//*.c" ?==) deps
            , arg "-o"
            , arg file ]

includeGccArgs :: Args
includeGccArgs = do
    path    <- getTargetPath
    pkgPath <- getPackagePath
    pkg     <- getPackage
    iDirs   <- getPkgDataList IncludeDirs
    dDirs   <- getPkgDataList DepIncludeDirs
    mconcat
        [ arg $ "-I" ++ path -/- "build/autogen"
        , append . map (\dir -> "-I" ++ pkgPath -/- dir) $ iDirs ++ dDirs ]

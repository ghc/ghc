module Settings.Builders.Gcc (gccArgs, gccMArgs) where

import Base
import Util
import Builder
import Expression
import Oracles.PackageData
import Settings.Util

-- TODO: check code duplication
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

-- TODO: handle custom $1_$2_MKDEPENDC_OPTS and
gccMArgs :: Args
gccMArgs = stagedBuilder GccM ? do
    path   <- getTargetPath
    file   <- getFile
    src    <- getDependency
    ccArgs <- getPkgDataList CcArgs
    mconcat
        [ arg "-E"
        , arg "-MM"
        , append ccArgs -- TODO: remove? any other flags?
        , includeGccArgs
        , arg "-MF"
        , arg file
        , arg "-x"
        , arg "c"
        , arg src ]

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

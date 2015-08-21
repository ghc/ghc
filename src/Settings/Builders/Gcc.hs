module Settings.Builders.Gcc (gccArgs, gccMArgs) where

import Base
import Util
import Builder
import Expression
import Predicates (stagedBuilder)
import Oracles.PackageData
import Settings.Util

-- TODO: check code duplication
gccArgs :: Args
gccArgs = stagedBuilder Gcc ? do
    file   <- getFile
    src    <- getSource
    ccArgs <- getPkgDataList CcArgs
    mconcat [ append ccArgs
            , includeGccArgs
            , arg "-c"
            , arg src
            , arg "-o"
            , arg file ]

-- TODO: handle custom $1_$2_MKDEPENDC_OPTS and
gccMArgs :: Args
gccMArgs = stagedBuilder GccM ? do
    file   <- getFile
    src    <- getSource
    ccArgs <- getPkgDataList CcArgs
    mconcat [ arg "-E"
            , arg "-MM"
            , append ccArgs -- TODO: remove? any other flags?
            , includeGccArgs
            , arg "-MF"
            , arg file
            , arg "-MT"
            , arg $ dropExtension file -<.> "o"
            , arg "-x"
            , arg "c"
            , arg src ]

includeGccArgs :: Args
includeGccArgs = do
    path    <- getTargetPath
    pkgPath <- getPackagePath
    iDirs   <- getPkgDataList IncludeDirs
    dDirs   <- getPkgDataList DepIncludeDirs
    mconcat
        [ arg $ "-I" ++ path -/- "build/autogen"
        , append . map (\dir -> "-I" ++ pkgPath -/- dir) $ iDirs ++ dDirs ]

module Settings.GccM (gccMArgs) where

import Util
import Builder
import Expression
import Oracles.PackageData
import Settings.Util

-- TODO: handle custom $1_$2_MKDEPENDC_OPTS and
gccMArgs :: Args
gccMArgs = stagedBuilder GccM ? do
    path   <- getTargetPath
    file   <- getFile
    ccArgs <- getPkgDataList CcArgs
    mconcat
        [ arg "-E"
        , arg "-MM"
        , append ccArgs -- TODO: remove? any other flags?
        , includeGccArgs
        , arg "-MF"
        , arg $ path -/- "build" -/- takeFileName file <.> "deps"
        , arg "-x"
        , arg "c"
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

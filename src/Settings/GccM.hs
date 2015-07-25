module Settings.GccM (gccMArgs) where

import Util
import Builder
import Package
import Expression
import Oracles.PackageData
import Settings.Util
import Settings.TargetDirectory

-- TODO: handle custom $1_$2_MKDEPENDC_OPTS and
gccMArgs :: Args
gccMArgs = do
    stage <- getStage
    builder (GccM stage) ? do
        pkg    <- getPackage
        files  <- getFiles
        ccArgs <- getPkgDataList CcArgs
        let file = head files
            path = targetPath stage pkg -/- "build"
        mconcat
            [ arg "-E"
            , arg "-MM"
            , append ccArgs -- TODO: remove? any other flags?
            , includeGccArgs
            , arg "-MF"
            , arg $ path -/- takeFileName file <.> "deps"
            , arg "-x"
            , arg "c"
            , arg file ]

includeGccArgs :: Args
includeGccArgs = do
    stage      <- getStage
    pkg        <- getPackage
    incDirs    <- getPkgDataList IncludeDirs
    depIncDirs <- getPkgDataList DepIncludeDirs
    let path = pkgPath pkg
    mconcat
        [ arg $ "-I" ++ targetPath stage pkg -/- "build/autogen"
        , append . map (\dir -> "-I" ++ path -/- dir) $ incDirs ++ depIncDirs ]

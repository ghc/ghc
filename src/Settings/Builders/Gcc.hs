module Settings.Builders.Gcc (gccArgs, gccMArgs) where

import Expression
import Oracles
import Predicates (stagedBuilder)
import Settings

gccArgs :: Args
gccArgs = stagedBuilder Gcc ? do
    file <- getFile
    src  <- getSource
    mconcat [ commonGccArgs
            , arg "-c"
            , arg src
            , arg "-o"
            , arg file ]

-- TODO: handle custom $1_$2_MKDEPENDC_OPTS and
gccMArgs :: Args
gccMArgs = stagedBuilder GccM ? do
    file <- getFile
    src  <- getSource
    mconcat [ arg "-E"
            , arg "-MM"
            , commonGccArgs
            , arg "-MF"
            , arg file
            , arg "-MT"
            , arg $ dropExtension file -<.> "o"
            , arg "-x"
            , arg "c"
            , arg src ]

commonGccArgs :: Args
commonGccArgs = do
    pkg    <- getPackage
    path   <- getTargetPath
    iDirs  <- getPkgDataList IncludeDirs
    dDirs  <- getPkgDataList DepIncludeDirs
    ccArgs <- getPkgDataList CcArgs
    mconcat [ append ccArgs
            , arg $ "-I" ++ path -/- "build/autogen"
            , append [ "-I" ++ pkgPath pkg -/- dir | dir <- iDirs ++ dDirs ]]

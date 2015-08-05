module Settings.GccM (gccMArgs) where

import Builder
import Expression
import Oracles.PackageData
import Settings.Gcc
import Settings.Util

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

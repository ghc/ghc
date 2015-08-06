module Settings.Builders.Ld (ldArgs) where

import Builder
import Expression
import Oracles.Setting
import Settings.Util

ldArgs :: Args
ldArgs = builder Ld ? do
    stage    <- getStage
    objs     <- getDependencies
    file     <- getFile
    confArgs <- getSettingList $ ConfLdLinkerArgs stage
    mconcat [ append confArgs
            , arg "-r"
            , arg "-o"
            , arg file
            , append objs ]

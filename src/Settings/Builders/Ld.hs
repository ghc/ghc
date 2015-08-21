module Settings.Builders.Ld (ldArgs) where

import Builder
import Expression
import Predicates (builder)
import Oracles.Setting
import Settings.Util

ldArgs :: Args
ldArgs = builder Ld ? do
    file <- getFile
    objs <- getSources
    mconcat [ argStagedSettingList ConfLdLinkerArgs
            , arg "-r"
            , arg "-o", arg file
            , append objs ]

module Settings.Builders.Ld (ldArgs) where

import Builder
import Expression
import Oracles
import Predicates (builder)
import Settings.Util

ldArgs :: Args
ldArgs = builder Ld ? do
    file <- getFile
    objs <- getSources
    mconcat [ argStagedSettingList ConfLdLinkerArgs
            , arg "-r"
            , arg "-o", arg file
            , append objs ]

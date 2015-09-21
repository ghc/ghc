module Settings.Builders.Happy (happyArgs) where

import Expression
import Predicates (builder)

happyArgs :: Args
happyArgs = builder Happy ? do
    src  <- getSource
    file <- getFile
    mconcat [ arg "-agc"
            , arg "--strict"
            , arg src
            , arg "-o", arg file ]

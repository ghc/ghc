module Settings.Builders.GenApply (
    genapplyBuilderArgs
    ) where

import Builder
import Settings.Builders.Common

genapplyBuilderArgs :: Args
genapplyBuilderArgs = builder GenApply ? do
    h <- getInput
    arg h

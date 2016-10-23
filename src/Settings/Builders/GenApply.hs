module Settings.Builders.GenApply (genApplyBuilderArgs) where

import Settings.Builders.Common

genApplyBuilderArgs :: Args
genApplyBuilderArgs = builder GenApply ? flag GhcUnregisterised ? arg "-u"

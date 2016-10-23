module Settings.Builders.GenApply (genApplyBuilderArgs) where

import Settings.Builders.Common

-- TODO: Dead code? ifeq "$(GhcUnregisterised)" "YES" GENAPPLY_OPTS = -u
genApplyBuilderArgs :: Args
genApplyBuilderArgs = mempty

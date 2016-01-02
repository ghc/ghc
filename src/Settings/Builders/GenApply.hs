module Settings.Builders.GenApply (genApplyBuilderArgs) where

import Expression

-- Stdin/stdout are handled in a special way. See Rules/Actions.hs.
-- TODO: Dead code? ifeq "$(GhcUnregisterised)" "YES" GENAPPLY_OPTS = -u
genApplyBuilderArgs :: Args
genApplyBuilderArgs = mempty

{-# LANGUAGE TypeFamilies #-}
module Distribution.Types.TargetInfo (
    TargetInfo(..)
) where

import Prelude ()
import Distribution.Compat.Prelude

import Distribution.Types.ComponentLocalBuildInfo
import Distribution.Types.Component
import Distribution.Types.UnitId

import Distribution.Compat.Graph (IsNode(..))

-- | The 'TargetInfo' contains all the information necessary to build a
-- specific target (e.g., component/module/file) in a package.  In
-- principle, one can get the 'Component' from a
-- 'ComponentLocalBuildInfo' and 'LocalBuildInfo', but it is much more
-- convenient to have the component in hand.
data TargetInfo = TargetInfo {
        targetCLBI      :: ComponentLocalBuildInfo,
        targetComponent :: Component
        -- TODO: BuildTargets supporting parsing these is dumb,
        -- we don't have support for compiling single modules or
        -- file paths. Accommodating it now is premature
        -- generalization.  Figure it out later.
        -- targetSub       :: Maybe (Either ModuleName FilePath)
    }

instance IsNode TargetInfo where
    type Key TargetInfo = UnitId
    nodeKey       = nodeKey       . targetCLBI
    nodeNeighbors = nodeNeighbors . targetCLBI

module Settings.Ways (
    ways
    ) where

import Base
import Ways hiding (defaultWays)
import Switches
import Expression
import UserSettings

-- Combining default ways with user modifications
ways :: Ways
ways = defaultWays <> userWays

-- These are default ways
defaultWays :: Ways
defaultWays = mconcat
    [                              append [vanilla] -- always build vanilla
    , notStage Stage0            ? append [profiling]
    , platformSupportsSharedLibs ? append [dynamic] ]

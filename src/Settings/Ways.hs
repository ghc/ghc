module Settings.Ways (
    ways
    ) where

import Way
import Stage
import Switches
import Expression
import Settings.User

-- Combining default ways with user modifications
ways :: Ways
ways = defaultWays <> userWays

-- These are default ways
defaultWays :: Ways
defaultWays = mconcat
    [                              append [vanilla] -- always build vanilla
    , notStage Stage0            ? append [profiling]
    , platformSupportsSharedLibs ? append [dynamic] ]

module Settings.Ways (getWays, getRtsWays) where

import Way
import Stage
import Expression
import Predicates
import Oracles
import Settings.User

-- Combining default ways with user modifications
getWays :: Expr [Way]
getWays = fromDiffExpr $ defaultWays <> userWays

getRtsWays :: Expr [Way]
getRtsWays = fromDiffExpr $ defaultRtsWays <> userRtsWays

-- These are default ways
defaultWays :: Ways
defaultWays = mconcat
    [                              append [vanilla] -- always build vanilla
    , notStage Stage0            ? append [profiling]
    , platformSupportsSharedLibs ? append [dynamic] ]

defaultRtsWays :: Ways
defaultRtsWays = do
    ways <- getWays
    mconcat
        [ append [ logging, debug, threaded, threadedDebug, threadedLogging ]
        , (profiling `elem` ways) ? append [threadedProfiling]
        , (dynamic `elem` ways) ?
          append [ dynamic, debugDynamic, threadedDynamic, threadedDebugDynamic
                 , loggingDynamic, threadedLoggingDynamic ] ]

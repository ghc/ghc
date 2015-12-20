module Settings.Ways (getWays, getLibWays, getRtsWays) where

import Expression
import Predicates
import Settings.User

-- TODO: use a single expression Ways parameterised by package instead of
-- expressions libWays and rtsWays

-- Combining default ways with user modifications
getLibWays :: Expr [Way]
getLibWays = fromDiffExpr $ defaultLibWays <> userLibWays

-- In Stage0 we only build vanilla
getWays :: Expr [Way]
getWays = mconcat [ stage0 ? return [vanilla], notStage0 ? getLibWays ]

getRtsWays :: Expr [Way]
getRtsWays = fromDiffExpr $ defaultRtsWays <> userRtsWays

-- These are default ways
defaultLibWays :: Ways
defaultLibWays = mconcat
    [ append [vanilla, profiling]
    , platformSupportsSharedLibs ? append [dynamic] ]

defaultRtsWays :: Ways
defaultRtsWays = do
    ways <- getLibWays
    mconcat
        [ append [ logging, debug, threaded, threadedDebug, threadedLogging ]
        , (profiling `elem` ways) ? append [threadedProfiling]
        , (dynamic `elem` ways) ?
          append [ dynamic, debugDynamic, threadedDynamic, threadedDebugDynamic
                 , loggingDynamic, threadedLoggingDynamic ] ]

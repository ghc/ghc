module Settings.Ways (getLibraryWays, getRtsWays) where

import Base
import Expression
import Predicates
import Settings.User
import Oracles.Config.Flag

-- | Combine default ways with user modifications
getLibraryWays :: Expr [Way]
getLibraryWays = fromDiffExpr $ defaultLibraryWays <> userLibraryWays

getRtsWays :: Expr [Way]
getRtsWays = fromDiffExpr $ defaultRtsWays <> userRtsWays

-- These are default ways for library packages:
-- * We always build 'vanilla' way.
-- * We build 'profiling' way when stage > Stage0.
-- * We build 'dynamic' way when stage > Stage0 and the platform supports it.
defaultLibraryWays :: Ways
defaultLibraryWays = mconcat
    [ append [vanilla]
    , notStage0 ? append [profiling]
    , notStage0 ? platformSupportsSharedLibs ? append [dynamic] ]

defaultRtsWays :: Ways
defaultRtsWays = do
    ways <- getLibraryWays
    mconcat
        [ append [ logging, debug, threaded, threadedDebug, threadedLogging ]
        , (profiling `elem` ways) ? append [threadedProfiling]
        , (dynamic `elem` ways) ?
          append [ dynamic, debugDynamic, threadedDynamic, threadedDebugDynamic
                 , loggingDynamic, threadedLoggingDynamic ] ]

{-# LANGUAGE NoImplicitPrelude #-}
module Ways (
    WayUnit (..),
    Way, tag, 
    
    allWays, defaultWays, 

    vanilla, profiling, logging, parallel, granSim, 
    threaded, threadedProfiling, threadedLogging, 
    debug, debugProfiling, threadedDebug, threadedDebugProfiling,
    dynamic, profilingDynamic, threadedProfilingDynamic,
    threadedDynamic, threadedDebugDynamic, debugDynamic,
    loggingDynamic, threadedLoggingDynamic,

    hisuf, osuf, hcsuf
    ) where

import Base
import Oracles

data WayUnit = Profiling | Logging | Parallel | GranSim | Threaded | Debug | Dynamic deriving Eq

data Way = Way
     {
         tag         :: String,    -- e.g., "thr_p"
         description :: String,    -- e.g., "threaded profiled"
         units       :: [WayUnit]  -- e.g., [Threaded, Profiling]
     }
     deriving Eq

vanilla   = Way "v"  "vanilla"       []
profiling = Way "p"  "profiling"     [Profiling]
logging   = Way "l"  "event logging" [Logging]
parallel  = Way "mp" "parallel"      [Parallel]
granSim   = Way "gm" "GranSim"       [GranSim]

-- RTS only ways

threaded                 = Way "thr"           "threaded"                       [Threaded]
threadedProfiling        = Way "thr_p"         "threaded profiling"             [Threaded, Profiling]
threadedLogging          = Way "thr_l"         "threaded event logging"         [Threaded, Logging]
debug                    = Way "debug"         "debug"                          [Debug]
debugProfiling           = Way "debug_p"       "debug profiling"                [Debug, Profiling]
threadedDebug            = Way "thr_debug"     "threaded debug"                 [Threaded, Debug]
threadedDebugProfiling   = Way "thr_debug_p"   "threaded debug profiling"       [Threaded, Debug, Profiling]
dynamic                  = Way "dyn"           "dyn"                            [Dynamic]
profilingDynamic         = Way "p_dyn"         "p_dyn"                          [Profiling, Dynamic]
threadedProfilingDynamic = Way "thr_p_dyn"     "thr_p_dyn"                      [Threaded, Profiling, Dynamic]
threadedDynamic          = Way "thr_dyn"       "thr_dyn"                        [Threaded, Dynamic]
threadedDebugDynamic     = Way "thr_debug_dyn" "thr_debug_dyn"                  [Threaded, Debug, Dynamic]
debugDynamic             = Way "debug_dyn"     "debug_dyn"                      [Debug, Dynamic]
loggingDynamic           = Way "l_dyn"         "event logging dynamic"          [Logging, Dynamic]
threadedLoggingDynamic   = Way "thr_l_dyn"     "threaded event logging dynamic" [Threaded, Logging, Dynamic]

allWays = [vanilla, profiling, logging, parallel, granSim, 
    threaded, threadedProfiling, threadedLogging, 
    debug, debugProfiling, threadedDebug, threadedDebugProfiling,
    dynamic, profilingDynamic, threadedProfilingDynamic,
    threadedDynamic, threadedDebugDynamic, debugDynamic,
    loggingDynamic, threadedLoggingDynamic]

-- TODO: what are ways 't' and 's'?
-- ALL_WAYS=v p t l s mp mg debug dyn thr thr_l p_dyn debug_dyn thr_dyn thr_p_dyn thr_debug_dyn thr_p thr_debug debug_p thr_debug_p l_dyn thr_l_dyn

defaultWays :: Stage -> Action [Way]
defaultWays stage = do
    sharedLibs <- test PlatformSupportsSharedLibs
    return $ [vanilla]
        ++ [profiling | stage /= Stage0] 
        ++ [dynamic   | sharedLibs     ]

wayHcOpts :: Way -> Args
wayHcOpts (Way _ _ units) =
    mconcat
    [ when (Dynamic `notElem` units) $ arg [ "-static" ]
    , when (Dynamic    `elem` units) $ arg [ "-fPIC", "-dynamic" ]
    , when (Threaded   `elem` units) $ arg [ "-optc-DTHREADED_RTS" ]
    , when (Debug      `elem` units) $ arg [ "-optc-DDEBUG"        ]
    , when (Profiling  `elem` units) $ arg [ "-prof"               ]
    , when (Logging    `elem` units) $ arg [ "-eventlog"           ]
    , when (Parallel   `elem` units) $ arg [ "-parallel"           ]
    , when (GranSim    `elem` units) $ arg [ "-gransim"            ]
    , when (units == [Debug] || units == [Debug, Dynamic]) $ arg [ "-ticky", "-DTICKY_TICKY" ]
    ]

suffix :: FilePath -> Way -> FilePath
suffix base (Way _ _ units) = 
    concat $
        ["p_"   | Profiling `elem` units] ++
        ["dyn_" | Dynamic   `elem` units] ++
        [base                           ]

hisuf, osuf, hcsuf :: Way -> FilePath
hisuf = suffix "hi"
osuf  = suffix "o"
hcsuf = suffix "hc"

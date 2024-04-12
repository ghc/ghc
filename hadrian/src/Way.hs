module Way (
    WayUnit (..), Way, wayUnit, addWayUnit, removeWayUnit, wayFromUnits, allWays,

    vanilla, profiling, dynamic, profilingDynamic, threaded, debug,
    threadedDebug, threadedProfiling, threadedDynamic,
    threadedDebugProfiling, threadedDebugDynamic, threadedProfilingDynamic, threadedDebugProfilingDynamic,
    debugProfiling, debugDynamic, debugProfilingDynamic,

    wayPrefix, waySuffix, hisuf, osuf, hcsuf, obootsuf, hibootsuf, ssuf
    ) where

import Way.Type

-- | Build default _vanilla_ way.
vanilla :: Way
vanilla = wayFromUnits []

-- | Build with profiling.
profiling :: Way
profiling = wayFromUnits [Profiling]

-- | Build with dynamic linking.
dynamic :: Way
dynamic = wayFromUnits [Dynamic]

-- | Build with profiling and dynamic linking.
profilingDynamic :: Way
profilingDynamic = wayFromUnits [Profiling, Dynamic]

-- RTS only ways below. See compiler/GHC/Driver/Session.hs.
-- | Build multithreaded RTS.
threaded :: Way
threaded = wayFromUnits [Threaded]

-- | Build RTS with debug information.
debug :: Way
debug = wayFromUnits [Debug]

-- | Various combinations of RTS only ways.
threadedDebug, threadedProfiling, threadedDynamic,
    threadedDebugProfiling, threadedDebugDynamic, threadedProfilingDynamic, threadedDebugProfilingDynamic,
    debugProfiling, debugDynamic, debugProfilingDynamic :: Way
threadedDebug            = wayFromUnits [Threaded, Debug]
threadedProfiling        = wayFromUnits [Threaded, Profiling]
threadedDynamic          = wayFromUnits [Threaded, Dynamic]
threadedDebugProfiling   = wayFromUnits [Threaded, Debug, Profiling]
threadedDebugDynamic     = wayFromUnits [Threaded, Debug, Dynamic]
threadedDebugProfilingDynamic = wayFromUnits [Threaded, Debug, Profiling, Dynamic]
threadedProfilingDynamic = wayFromUnits [Threaded, Profiling, Dynamic]
debugProfiling           = wayFromUnits [Debug, Profiling]
debugDynamic             = wayFromUnits [Debug, Dynamic]
debugProfilingDynamic    = wayFromUnits [Debug, Profiling, Dynamic]
-- | All ways supported by the build system.
allWays :: [Way]
allWays =
    [ vanilla, profiling, dynamic, profilingDynamic, threaded, debug
    , threadedDebug, threadedProfiling, threadedDynamic
    , threadedDebugProfiling, threadedDebugDynamic, threadedProfilingDynamic
    , debugProfiling, debugDynamic ]

wayPrefix :: Way -> String
wayPrefix way | way == vanilla = ""
              | otherwise      = show way ++ "_"

waySuffix :: Way -> String
waySuffix way | way == vanilla = ""
              | otherwise      = "_" ++ show way

osuf, ssuf, hisuf, hcsuf, obootsuf, hibootsuf :: Way -> String
osuf      = (++ "o"      ) . wayPrefix
ssuf      = (++ "s"      ) . wayPrefix
hisuf     = (++ "hi"     ) . wayPrefix
hcsuf     = (++ "hc"     ) . wayPrefix
obootsuf  = (++ "o-boot" ) . wayPrefix
hibootsuf = (++ "hi-boot") . wayPrefix

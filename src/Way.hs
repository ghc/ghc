module Way (
    WayUnit (..), Way, wayUnit, removeWayUnit, wayFromUnits, allWays,

    vanilla, profiling, dynamic, profilingDynamic, threaded, debug, logging,
    threadedDebug, threadedProfiling, threadedLogging, threadedDynamic,
    threadedDebugProfiling, threadedDebugDynamic, threadedProfilingDynamic,
    threadedLoggingDynamic, debugProfiling, debugDynamic, loggingDynamic,

    wayPrefix, waySuffix, hisuf, osuf, hcsuf, obootsuf, hibootsuf, ssuf
    ) where

import Data.IntSet (IntSet)
import qualified Data.IntSet as Set
import Data.List
import Data.Maybe
import Development.Shake.Classes
import Hadrian.Utilities

-- Note: order of constructors is important for compatibility with the old build
-- system, e.g. we want "thr_p", not "p_thr" (see instance Show Way).
-- | A 'WayUnit' is a single way of building source code, for example with
-- profiling enabled, or dynamically linked.
data WayUnit = Threaded
             | Debug
             | Profiling
             | Logging
             | Dynamic
             deriving (Bounded, Enum, Eq, Ord)

-- TODO: get rid of non-derived Show instances
instance Show WayUnit where
    show unit = case unit of
        Threaded  -> "thr"
        Debug     -> "debug"
        Profiling -> "p"
        Logging   -> "l"
        Dynamic   -> "dyn"

instance Read WayUnit where
    readsPrec _ s = [(unit, "") | unit <- [minBound ..], show unit == s]

-- | Collection of 'WayUnit's that stands for the different ways source code
-- is to be built.
newtype Way = Way IntSet

instance Binary Way where
    put = put . show
    get = fmap read get

instance Hashable Way where
    hashWithSalt salt = hashWithSalt salt . show

instance NFData Way where
    rnf (Way s) = s `seq` ()

-- | Construct a 'Way' from multiple 'WayUnit's. Inverse of 'wayToUnits'.
wayFromUnits :: [WayUnit] -> Way
wayFromUnits = Way . Set.fromList . map fromEnum

-- | Split a 'Way' into its 'WayUnit' building blocks.
-- Inverse of 'wayFromUnits'.
wayToUnits :: Way -> [WayUnit]
wayToUnits (Way set) = map toEnum . Set.elems $ set

-- | Check whether a 'Way' contains a certain 'WayUnit'.
wayUnit :: WayUnit -> Way -> Bool
wayUnit unit (Way set) = fromEnum unit `Set.member` set

-- | Remove a 'WayUnit' from 'Way'.
removeWayUnit :: WayUnit -> Way -> Way
removeWayUnit unit (Way set) = Way . Set.delete (fromEnum unit) $ set

instance Show Way where
    show way = if null tag then "v" else tag
      where
        tag = intercalate "_" . map show . wayToUnits $ way

instance Read Way where
    readsPrec _ s = if s == "v" then [(vanilla, "")] else result
      where
        uniqueReads token = case reads token of
            [(unit, "")] -> Just unit
            _            -> Nothing
        units  = map uniqueReads . words . replaceEq '_' ' ' $ s
        result = if Nothing `elem` units
                 then []
                 else [(wayFromUnits . map fromJust $ units, "")]

instance Eq Way where
    Way a == Way b = a == b

instance Ord Way where
    compare (Way a) (Way b) = compare a b

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

-- RTS only ways below. See compiler/main/DynFlags.hs.
-- | Build RTS with event logging.
logging :: Way
logging = wayFromUnits [Logging]

-- | Build multithreaded RTS.
threaded :: Way
threaded = wayFromUnits [Threaded]

-- | Build RTS with debug information.
debug :: Way
debug = wayFromUnits [Debug]

-- | Various combinations of RTS only ways.
threadedDebug, threadedProfiling, threadedLogging, threadedDynamic,
    threadedDebugProfiling, threadedDebugDynamic, threadedProfilingDynamic,
    threadedLoggingDynamic, debugProfiling, debugDynamic, loggingDynamic :: Way
threadedDebug            = wayFromUnits [Threaded, Debug]
threadedProfiling        = wayFromUnits [Threaded, Profiling]
threadedLogging          = wayFromUnits [Threaded, Logging]
threadedDynamic          = wayFromUnits [Threaded, Dynamic]
threadedDebugProfiling   = wayFromUnits [Threaded, Debug, Profiling]
threadedDebugDynamic     = wayFromUnits [Threaded, Debug, Dynamic]
threadedProfilingDynamic = wayFromUnits [Threaded, Profiling, Dynamic]
threadedLoggingDynamic   = wayFromUnits [Threaded, Logging, Dynamic]
debugProfiling           = wayFromUnits [Debug, Profiling]
debugDynamic             = wayFromUnits [Debug, Dynamic]
loggingDynamic           = wayFromUnits [Logging, Dynamic]

-- | All ways supported by the build system.
allWays :: [Way]
allWays =
    [ vanilla, profiling, dynamic, profilingDynamic, threaded, debug, logging
    , threadedDebug, threadedProfiling, threadedLogging, threadedDynamic
    , threadedDebugProfiling, threadedDebugDynamic, threadedProfilingDynamic
    , threadedLoggingDynamic, debugProfiling, debugDynamic, loggingDynamic ]

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

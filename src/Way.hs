module Way ( -- TODO: rename to "Way"?
    WayUnit (..),
    Way, wayFromUnits, wayUnit,

    vanilla, profiling, logging, parallel, granSim,
    threaded, threadedProfiling, threadedLogging,
    debug, debugProfiling, threadedDebug, threadedDebugProfiling,
    dynamic, profilingDynamic, threadedProfilingDynamic,
    threadedDynamic, threadedDebugDynamic, debugDynamic,
    loggingDynamic, threadedLoggingDynamic,

    wayPrefix, hisuf, osuf, hcsuf, obootsuf, ssuf, libsuf,
    detectWay
    ) where

import Util
import Oracles.Setting
import Control.Applicative
import Development.Shake
import Development.Shake.Classes
import Data.List hiding (delete)
import Data.IntSet (IntSet, elems, member, delete, fromList)

data WayUnit = Threaded
             | Debug
             | Profiling
             | Logging
             | Dynamic
             | Parallel
             | GranSim
             deriving Enum

instance Show WayUnit where
    show unit = case unit of
        Threaded  -> "thr"
        Debug     -> "debug"
        Profiling -> "p"
        Logging   -> "l"
        Dynamic   -> "dyn"
        Parallel  -> "mp"
        GranSim   -> "gm"

instance Read WayUnit where
    readsPrec _ s = [(unit, "") | unit <- [Threaded ..], show unit == s]

newtype Way = Way IntSet

wayFromUnits :: [WayUnit] -> Way
wayFromUnits = Way . fromList . map fromEnum

wayToUnits :: Way -> [WayUnit]
wayToUnits (Way set) = map toEnum . elems $ set

wayUnit :: WayUnit -> Way -> Bool
wayUnit unit (Way set) = fromEnum unit `member` set

instance Show Way where
    show way = if null tag then "v" else tag
      where
        tag = intercalate "_" . map show . wayToUnits $ way

instance Read Way where
    readsPrec _ s =
        if s == "v"
        then [(vanilla, "")]
        else [(wayFromUnits . map read . words . replaceEq '_' ' ' $ s, "")]

instance Eq Way where
    Way a == Way b = a == b

vanilla   = wayFromUnits []
profiling = wayFromUnits [Profiling]
logging   = wayFromUnits [Logging]
parallel  = wayFromUnits [Parallel]
granSim   = wayFromUnits [GranSim]

-- RTS only ways
-- TODO: do we need to define *only* these? Shall we generalise/simplify?
threaded                 = wayFromUnits [Threaded]
threadedProfiling        = wayFromUnits [Threaded, Profiling]
threadedLogging          = wayFromUnits [Threaded, Logging]
debug                    = wayFromUnits [Debug]
debugProfiling           = wayFromUnits [Debug, Profiling]
threadedDebug            = wayFromUnits [Threaded, Debug]
threadedDebugProfiling   = wayFromUnits [Threaded, Debug, Profiling]
dynamic                  = wayFromUnits [Dynamic]
profilingDynamic         = wayFromUnits [Profiling, Dynamic]
threadedProfilingDynamic = wayFromUnits [Threaded, Profiling, Dynamic]
threadedDynamic          = wayFromUnits [Threaded, Dynamic]
threadedDebugDynamic     = wayFromUnits [Threaded, Debug, Dynamic]
debugDynamic             = wayFromUnits [Debug, Dynamic]
loggingDynamic           = wayFromUnits [Logging, Dynamic]
threadedLoggingDynamic   = wayFromUnits [Threaded, Logging, Dynamic]

wayPrefix :: Way -> String
wayPrefix way | way == vanilla = ""
              | otherwise      = show way ++ "_"

hisuf, osuf, hcsuf, obootsuf, ssuf :: Way -> String
osuf     = (++ "o"     ) . wayPrefix
ssuf     = (++ "s"     ) . wayPrefix
hisuf    = (++ "hi"    ) . wayPrefix
hcsuf    = (++ "hc"    ) . wayPrefix
obootsuf = (++ "o-boot") . wayPrefix

-- Note: in the previous build system libsuf was mysteriously different
-- from other suffixes. For example, in the profiling way it used to be
-- "_p.a" instead of ".p_a" which is how other suffixes work. I decided
-- to make all suffixes consistent: ".way_extension".
-- TODO: find out why we need version number in the dynamic suffix
-- The current theory: dynamic libraries are eventually placed in a single
-- giant directory in the load path of the dynamic linker, and hence we must
-- distinguish different versions of GHC. In contrast static libraries live
-- in their own per-package directory and hence do not need a unique filename.
-- We also need to respect the system's dynamic extension, e.g. .dll or .so.
libsuf :: Way -> Action String
libsuf way @ (Way set) =
    if (not . wayUnit Dynamic $ way)
    then return $ wayPrefix way ++ "a" -- e.g., p_a
    else do
        extension <- setting DynamicExtension  -- e.g., .dll or .so
        version   <- setting ProjectVersion    -- e.g., 7.11.20141222
        let prefix = wayPrefix . Way . delete (fromEnum Dynamic) $ set
        -- e.g., p_ghc7.11.20141222.dll (the result)
        return $ prefix ++ "ghc" ++ version ++ extension

-- Detect way from a given file extension. Fails if there is no match.
detectWay :: FilePath -> Way
detectWay extension = read prefix
  where
    prefix = dropWhileEnd (== '_') . dropWhileEnd (/= '_') $ extension

-- Instances for storing in the Shake database
instance Binary Way where
    put = put . show
    get = read <$> get

instance Hashable Way where
    hashWithSalt salt = hashWithSalt salt . show

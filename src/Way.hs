module Way (
    WayUnit (..), Way, wayUnit,

    vanilla, profiling, logging, parallel, granSim,
    threaded, threadedProfiling, threadedLogging,
    debug, debugProfiling, threadedDebug, threadedDebugProfiling,
    dynamic, profilingDynamic, threadedProfilingDynamic,
    threadedDynamic, threadedDebugDynamic, debugDynamic,
    loggingDynamic, threadedLoggingDynamic,

    wayPrefix, hisuf, osuf, hcsuf, obootsuf, hibootsuf, ssuf, libsuf,
    safeDetectWay, detectWay, matchBuildResult
    ) where

import Base hiding (unit)
import Data.IntSet (IntSet)
import qualified Data.IntSet as Set
import Oracles

-- Note: order of constructors is important for compatibility with the old build
-- system, e.g. we want "thr_p", not "p_thr" (see instance Show Way).
data WayUnit = Threaded
             | Debug
             | Profiling
             | Logging
             | Dynamic
             | Parallel
             | GranSim
             deriving (Eq, Enum)

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
wayFromUnits = Way . Set.fromList . map fromEnum

wayToUnits :: Way -> [WayUnit]
wayToUnits (Way set) = map toEnum . Set.elems $ set

wayUnit :: WayUnit -> Way -> Bool
wayUnit unit (Way set) = fromEnum unit `Set.member` set

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

vanilla, profiling, logging, parallel, granSim :: Way
vanilla   = wayFromUnits []
profiling = wayFromUnits [Profiling]
logging   = wayFromUnits [Logging]
parallel  = wayFromUnits [Parallel]
granSim   = wayFromUnits [GranSim]

-- RTS only ways
-- TODO: do we need to define *only* these? Shall we generalise/simplify?
threaded, threadedProfiling, threadedLogging, debug, debugProfiling,
    threadedDebug, threadedDebugProfiling, dynamic, profilingDynamic,
    threadedProfilingDynamic, threadedDynamic, threadedDebugDynamic,
    debugDynamic, loggingDynamic, threadedLoggingDynamic :: Way

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

osuf, ssuf, hisuf, hcsuf, obootsuf, hibootsuf :: Way -> String
osuf      = (++ "o"      ) . wayPrefix
ssuf      = (++ "s"      ) . wayPrefix
hisuf     = (++ "hi"     ) . wayPrefix
hcsuf     = (++ "hc"     ) . wayPrefix
obootsuf  = (++ "o-boot" ) . wayPrefix
hibootsuf = (++ "hi-boot") . wayPrefix

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
        let prefix = wayPrefix . Way . Set.delete (fromEnum Dynamic) $ set
        -- e.g., p_ghc7.11.20141222.dll (the result)
        return $ prefix ++ "ghc" ++ version ++ extension

-- Detect way from a given filename. Returns Nothing if there is no match:
-- * safeDetectWay "foo/bar.hi"                == Just vanilla
-- * safeDetectWay "baz.thr_p_o"               == Just threadedProfiling
-- * safeDetectWay "qwe.ph_i"                  == Nothing (expected "qwe.p_hi")
-- * safeDetectWay "xru.p_ghc7.11.20141222.so" == Just profiling
safeDetectWay :: FilePath -> Maybe Way
safeDetectWay file = case reads prefix of
    [(way, "")] -> Just way
    _           -> Nothing
  where
    extension = takeExtension file
    prefixed  = if extension `notElem` [".so", ".dll", ".dynlib"]
                then extension
                else takeExtension . dropExtension .
                     dropExtension . dropExtension $ file
    prefix = drop 1 . dropWhileEnd (== '_') . dropWhileEnd (/= '_') $ prefixed

-- Unsafe version of safeDetectWay. Useful when matchBuildResult has succeeded.
detectWay :: FilePath -> Way
detectWay = fromJust . safeDetectWay

-- Given a path, an extension suffix, and a file name check:
-- 1) the file conforms to pattern 'path//*suffix'
-- 2) file's extension has a valid way tag (i.e., safeDetectWay does not fail)
matchBuildResult :: FilePath -> String -> FilePath -> Bool
matchBuildResult path suffix file =
    (path <//> "*" ++ suffix) ?== file && isJust (safeDetectWay file)

-- Instances for storing in the Shake database
instance Binary Way where
    put = put . show
    get = fmap read get

instance Hashable Way where
    hashWithSalt salt = hashWithSalt salt . show

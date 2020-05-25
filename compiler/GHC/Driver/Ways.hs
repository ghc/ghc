-- | Ways
--
-- The central concept of a "way" is that all objects in a given
-- program must be compiled in the same "way". Certain options change
-- parameters of the virtual machine, eg. profiling adds an extra word
-- to the object header, so profiling objects cannot be linked with
-- non-profiling objects.
--
-- After parsing the command-line options, we determine which "way" we
-- are building - this might be a combination way, eg. profiling+threaded.
--
-- There are two kinds of ways:
--    - RTS only: only affect the runtime system (RTS) and don't affect code
--    generation (e.g. threaded, debug)
--    - Full ways: affect code generation and the RTS (e.g. profiling, dynamic
--    linking)
--
-- We then find the "build-tag" associated with this way, and this
-- becomes the suffix used to find .hi files and libraries used in
-- this compilation.
module GHC.Driver.Ways
   ( Way(..)
   , hasWay
   , allowed_combination
   , wayGeneralFlags
   , wayUnsetGeneralFlags
   , wayOptc
   , wayOptl
   , wayOptP
   , wayDesc
   , wayRTSOnly
   , wayTag
   , waysTag
   , waysBuildTag
   -- * Host GHC ways
   , hostFullWays
   , hostIsProfiled
   , hostIsDynamic
   )
where

import GHC.Prelude
import GHC.Platform
import GHC.Driver.Flags

import qualified Data.Set as Set
import Data.Set (Set)
import Data.List (intersperse)
import System.IO.Unsafe ( unsafeDupablePerformIO )

-- | A way
--
-- Don't change the constructor order as it us used by `waysTag` to create a
-- unique tag (e.g. thr_debug_p) which is expected by other tools (e.g. Cabal).
data Way
  = WayCustom String -- ^ for GHC API clients building custom variants
  | WayThreaded      -- ^ (RTS only) Multithreaded runtime system
  | WayDebug         -- ^ Debugging, enable trace messages and extra checks
  | WayProf          -- ^ Profiling, enable cost-centre stacks and profiling reports
  | WayEventLog      -- ^ (RTS only) enable event logging
  | WayDyn           -- ^ Dynamic linking
  deriving (Eq, Ord, Show)

-- | Test if a ways is enabled
hasWay :: Set Way -> Way -> Bool
hasWay ws w = Set.member w ws

-- | Check if a combination of ways is allowed
allowed_combination :: Set Way -> Bool
allowed_combination ways = not disallowed
  where
   disallowed = or [ hasWay ways x && hasWay ways y
                   | (x,y) <- couples
                   ]
   -- List of disallowed couples of ways
   couples = [] -- we don't have any disallowed combination of ways nowadays

-- | Unique tag associated to a list of ways
waysTag :: Set Way -> String
waysTag = concat . intersperse "_" . map wayTag . Set.toAscList

-- | Unique build-tag associated to a list of ways
--
-- RTS only ways are filtered out because they have no impact on the build.
waysBuildTag :: Set Way -> String
waysBuildTag ws = waysTag (Set.filter (not . wayRTSOnly) ws)


-- | Unique build-tag associated to a way
wayTag :: Way -> String
wayTag (WayCustom xs) = xs
wayTag WayThreaded    = "thr"
wayTag WayDebug       = "debug"
wayTag WayDyn         = "dyn"
wayTag WayProf        = "p"
wayTag WayEventLog    = "l"

-- | Return true for ways that only impact the RTS, not the generated code
wayRTSOnly :: Way -> Bool
wayRTSOnly (WayCustom {}) = False
wayRTSOnly WayDyn         = False
wayRTSOnly WayProf        = False
wayRTSOnly WayThreaded    = True
wayRTSOnly WayDebug       = True
wayRTSOnly WayEventLog    = True

wayDesc :: Way -> String
wayDesc (WayCustom xs) = xs
wayDesc WayThreaded    = "Threaded"
wayDesc WayDebug       = "Debug"
wayDesc WayDyn         = "Dynamic"
wayDesc WayProf        = "Profiling"
wayDesc WayEventLog    = "RTS Event Logging"

-- | Turn these flags on when enabling this way
wayGeneralFlags :: Platform -> Way -> [GeneralFlag]
wayGeneralFlags _ (WayCustom {}) = []
wayGeneralFlags _ WayThreaded = []
wayGeneralFlags _ WayDebug    = []
wayGeneralFlags _ WayDyn      = [Opt_PIC, Opt_ExternalDynamicRefs]
    -- We could get away without adding -fPIC when compiling the
    -- modules of a program that is to be linked with -dynamic; the
    -- program itself does not need to be position-independent, only
    -- the libraries need to be.  HOWEVER, GHCi links objects into a
    -- .so before loading the .so using the system linker.  Since only
    -- PIC objects can be linked into a .so, we have to compile even
    -- modules of the main program with -fPIC when using -dynamic.
wayGeneralFlags _ WayProf     = []
wayGeneralFlags _ WayEventLog = []

-- | Turn these flags off when enabling this way
wayUnsetGeneralFlags :: Platform -> Way -> [GeneralFlag]
wayUnsetGeneralFlags _ (WayCustom {}) = []
wayUnsetGeneralFlags _ WayThreaded = []
wayUnsetGeneralFlags _ WayDebug    = []
wayUnsetGeneralFlags _ WayDyn      = [Opt_SplitSections]
   -- There's no point splitting when we're going to be dynamically linking.
   -- Plus it breaks compilation on OSX x86.
wayUnsetGeneralFlags _ WayProf     = []
wayUnsetGeneralFlags _ WayEventLog = []

-- | Pass these options to the C compiler when enabling this way
wayOptc :: Platform -> Way -> [String]
wayOptc _ (WayCustom {}) = []
wayOptc platform WayThreaded = case platformOS platform of
                               OSOpenBSD -> ["-pthread"]
                               OSNetBSD  -> ["-pthread"]
                               _         -> []
wayOptc _ WayDebug      = []
wayOptc _ WayDyn        = []
wayOptc _ WayProf       = ["-DPROFILING"]
wayOptc _ WayEventLog   = ["-DTRACING"]

-- | Pass these options to linker when enabling this way
wayOptl :: Platform -> Way -> [String]
wayOptl _ (WayCustom {}) = []
wayOptl platform WayThreaded =
   case platformOS platform of
   -- N.B. FreeBSD cc throws a warning if we pass -pthread without
   -- actually using any pthread symbols.
   OSFreeBSD  -> ["-pthread", "-Wno-unused-command-line-argument"]
   OSOpenBSD  -> ["-pthread"]
   OSNetBSD   -> ["-pthread"]
   _          -> []
wayOptl _ WayDebug      = []
wayOptl _ WayDyn        = []
wayOptl _ WayProf       = []
wayOptl _ WayEventLog   = []

-- | Pass these options to the preprocessor when enabling this way
wayOptP :: Platform -> Way -> [String]
wayOptP _ (WayCustom {}) = []
wayOptP _ WayThreaded = []
wayOptP _ WayDebug    = []
wayOptP _ WayDyn      = []
wayOptP _ WayProf     = ["-DPROFILING"]
wayOptP _ WayEventLog = ["-DTRACING"]


-- | Consult the RTS to find whether it has been built with profiling enabled.
hostIsProfiled :: Bool
hostIsProfiled = unsafeDupablePerformIO rtsIsProfiledIO /= 0

foreign import ccall unsafe "rts_isProfiled" rtsIsProfiledIO :: IO Int

-- | Consult the RTS to find whether GHC itself has been built with
-- dynamic linking.  This can't be statically known at compile-time,
-- because we build both the static and dynamic versions together with
-- -dynamic-too.
hostIsDynamic :: Bool
hostIsDynamic = unsafeDupablePerformIO rtsIsDynamicIO /= 0

foreign import ccall unsafe "rts_isDynamic" rtsIsDynamicIO :: IO Int

-- | Return host "full" ways (i.e. ways that have an impact on the compilation,
-- not RTS only ways). These ways must be used when compiling codes targeting
-- the internal interpreter.
hostFullWays :: Set Way
hostFullWays = Set.unions
   [ if hostIsDynamic  then Set.singleton WayDyn  else Set.empty
   , if hostIsProfiled then Set.singleton WayProf else Set.empty
   ]

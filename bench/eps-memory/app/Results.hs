-- | Result data shared between eps-analyse and eps-compare.
-- Stored with Show/Read so we do not need a JSON dependency.
module Results
  ( Stats(..)
  , FieldResult(..)
  , Results(..)
  , RtsStats
  , FragResult(..)
  , GroupClass(..)
  , readResults
  , writeResults
  , readRtsStats
  , fmtBytes
  , fmtDelta
  ) where

import qualified Data.Map.Strict as Map
import Text.Printf (printf)

-- | Closure count and total size in bytes.
data Stats = Stats
  { stCount :: !Int
  , stBytes :: !Int
  } deriving (Show, Read, Eq)

instance Semigroup Stats where
  Stats a b <> Stats c d = Stats (a + c) (b + d)

instance Monoid Stats where
  mempty = Stats 0 0

data FieldResult = FieldResult
  { frName       :: String
  , frReachable  :: Stats
    -- ^ everything reachable from this field (shared structure included)
  , frExclusive  :: Int
    -- ^ bytes reachable from this field but from no other EPS field
  , frArrWords   :: Stats
    -- ^ ARR_WORDS (byte arrays) reachable from this field
  , frBigArrWords :: Stats
    -- ^ ARR_WORDS of at least 'bigArrWordsBytes' bytes (e.g. whole .hi buffers)
  , frTopTypes   :: [(String, Stats)]
    -- ^ largest closure types reachable from this field
  } deriving (Show, Read)

type RtsStats = Map.Map String String

-- | Pinned-block fragmentation at the end of the build (ghc-debug).
-- "live" = reachable from the saved roots (EPS and HscEnv). A pinned block
-- group is kept by the GC as a whole while any object in it is live.
data FragResult = FragResult
  { fgLabel      :: String
  , fgBlocks     :: Int     -- ^ all block groups the RTS reported
  , fgBlockBytes :: Int     -- ^ their total size
  , fgGroups     :: [GroupClass]
    -- ^ pinned block groups, by kind (single 4 KiB block / multi-block)
  } deriving (Show, Read)

data GroupClass = GroupClass
  { gcKind      :: String      -- ^ "1 block" or "multi-block"
  , gcCount     :: Int
  , gcBytes     :: Int         -- ^ total size of the groups
  , gcLive      :: Stats       -- ^ live objects in them
  , gcHist      :: [(Int, (Int, Int))]
    -- ^ utilisation decile 0..9 -> (#groups, bytes of these groups)
  , gcObjSizes  :: [(Int, Int)]
    -- ^ live objects by size bucket (upper bound in bytes) -> count
  } deriving (Show, Read)

data Results = Results
  { rsLabel          :: String
  , rsEpsReachable   :: Stats
  , rsEpsArrWords    :: Stats
  , rsEpsBigArrWords :: Stats
  , rsLiveHeap       :: Stats
    -- ^ everything reachable from the saved roots (EPS and HscEnv)
  , rsEpsClosures    :: Int
    -- ^ number of EPS constructors reachable from the saved roots
  , rsLiveBytes      :: Maybe Int
    -- ^ RTS live bytes after a major GC at the pause point
  , rsFields         :: [FieldResult]
  , rsRts            :: RtsStats
    -- ^ +RTS -t --machine-readable stats of an unpaused run
  } deriving (Show, Read)

readResults :: FilePath -> IO Results
readResults fp = read <$> readFile fp

writeResults :: FilePath -> Results -> IO ()
writeResults fp r = writeFile fp (show r)

-- | Parse the output of @+RTS -t<file> --machine-readable@: the first line is
-- the command line, the rest is a Haskell list of string pairs.
readRtsStats :: FilePath -> IO RtsStats
readRtsStats fp = do
  s <- readFile fp
  let body = unlines (drop 1 (lines s))
  case reads body of
    [(kvs, _)] -> pure (Map.fromList kvs)
    _          -> pure Map.empty

-- | Bytes in MiB (2^20), with MB (10^6, as used e.g. by eventlog2html) in
-- parentheses for values of at least 1 MiB.
fmtBytes :: Int -> String
fmtBytes b
  | abs b >= 1024 * 1024 = printf "%.1f MiB [%.1f MB]" (fromIntegral b / (1024 * 1024) :: Double)
                                                        (fromIntegral b / 1e6 :: Double)
  | abs b >= 1024        = printf "%.1f KiB" (fromIntegral b / 1024 :: Double)
  | otherwise            = printf "%d B" b

-- | The new value, with its change relative to the baseline in percent.
fmtDelta :: Int -> Int -> String
fmtDelta base new
  | base == 0 = fmtBytes new
  | otherwise = printf "%s (%+.1f%%)" (fmtBytes new)
                  (100 * fromIntegral (new - base) / fromIntegral base :: Double)

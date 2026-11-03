{-# LANGUAGE OverloadedStrings #-}
-- | eps-peak: find out what the peak memory of a GHC run consists of.
--
-- Input: an eventlog of a ghc run with
--   ghc ... -ddump-timings -ddump-to-file +RTS -l -hT -i<interval> -RTS
-- (-ddump-timings makes GHC emit "GHC:started: <phase> [<Module>]" markers).
--
-- Usage:
--   eps-peak analyse EVENTLOG LABEL OUT_PREFIX [TOP]   -- writes OUT_PREFIX.md, OUT_PREFIX.hsdat
--   eps-peak compare BASE.hsdat NEW.hsdat [TOP]       -- markdown to stdout
module Main (main) where

import Data.List (sortOn)
import qualified Data.Map.Strict as Map
import Data.Ord (Down(..))
import qualified Data.Text as T
import System.Environment (getArgs)
import System.Exit (die)
import Text.Printf (printf)
import System.IO (hSetEncoding, stdout, utf8)

import GHC.RTS.Events

import Results (fmtBytes, fmtDelta)

data PeakResult = PeakResult
  { prLabel      :: String
  , prPeakLive   :: Int
    -- ^ maximum live bytes after a major GC
  , prPeakTime   :: Double
    -- ^ seconds since start
  , prPeakPhases :: [String]
    -- ^ GHC phases running at the peak, innermost first
  , prTopGCs     :: [(Double, Int, [String])]
    -- ^ biggest major GCs: time, live bytes, phases
  , prPerModule  :: [(String, Int)]
    -- ^ max live bytes after a major GC while a module was being processed
  , prCensusTime :: Double
  , prCensus     :: [(String, Int)]
    -- ^ -hT census (closure type, bytes) closest to the peak
  , prTotalTime  :: Double
  , prMaxLiveAll :: (Double, Int)
    -- ^ maximum HeapLive over all GCs (minor ones included: after a minor GC,
    -- the old generation is counted as live without being traced)
  , prMaxCensus  :: (Double, Int)
    -- ^ largest -hT census total
  , prMaxHeapSize :: (Double, Int)
    -- ^ maximum HeapSize (memory the RTS holds for the heap)
  , prBlocksAtPeak :: Int
    -- ^ BlocksSize (memory in allocated blocks) at the major-GC peak
  , prMaxOverhead :: (Double, Int, Int)
    -- ^ major GC with the largest BlocksSize - HeapLive: time, blocks, live
  } deriving (Show, Read)

main :: IO ()
main = do
  hSetEncoding stdout utf8
  args <- getArgs
  case args of
    ["analyse", el, lbl, out]      -> analyse el lbl out 30
    ["analyse", el, lbl, out, top] -> analyse el lbl out (read top)
    ["compare", a, b]              -> compareR a b 30
    ["compare", a, b, top]         -> compareR a b (read top)
    _ -> die "usage: eps-peak analyse EVENTLOG LABEL OUT [TOP] | eps-peak compare A.hsdat B.hsdat [TOP]"

data St = St
  { stOpen    :: [T.Text]          -- open phases, innermost first
  , stLastGen :: Int
  , stMajors  :: [(Double, Int, [String])]
  , stPerMod  :: Map.Map String Int
  , stSample  :: Maybe (Double, Map.Map String Int)  -- sample being read
  , stSamples :: [(Double, Map.Map String Int)]
  , stLast    :: Double
  , stMaxLive :: (Double, Int)
  , stMaxSize :: (Double, Int)
  , stBlocks  :: [(Double, Int)]
  }

secs :: Timestamp -> Double
secs t = fromIntegral t / 1e9

analyse :: FilePath -> String -> FilePath -> Int -> IO ()
analyse el lbl out top = do
  r <- readEventLogFromFile el
  EventLog _ (Data evs) <- either die pure r
  let st = foldl' step (St [] 0 [] Map.empty Nothing [] 0 (0, 0) (0, 0) []) (sortEvents evs)
      majors = stMajors st
  (pt, plive, pphases) <- case sortOn (\(_, l, _) -> Down l) majors of
    (m:_) -> pure m
    []    -> die "no major GCs with HeapLive events in the eventlog"
  let (ct, census) = case sortOn (\(t, _) -> abs (t - pt)) (stSamples st) of
        (s:_) -> s
        []    -> (0, Map.empty)
      res = PeakResult
        { prLabel = lbl
        , prPeakLive = plive
        , prPeakTime = pt
        , prPeakPhases = pphases
        , prTopGCs = take 10 (sortOn (\(_, l, _) -> Down l) majors)
        , prPerModule = take top (sortOn (Down . snd) (Map.toList (stPerMod st)))
        , prCensusTime = ct
        , prCensus = take (max top 60) (sortOn (Down . snd) (Map.toList census))
        , prTotalTime = stLast st
        , prMaxLiveAll = stMaxLive st
        , prMaxCensus = case sortOn (Down . snd)
                               [ (t, sum (Map.elems m)) | (t, m) <- stSamples st ] of
                          (c:_) -> c
                          []    -> (0, 0)
        , prMaxHeapSize = stMaxSize st
        , prBlocksAtPeak = blocksAt pt
        , prMaxOverhead = case sortOn (\(_, b, l) -> Down (b - l))
                                 [ (t, blocksAt t, l) | (t, l, _) <- majors ] of
                            (x:_) -> x
                            []    -> (0, 0, 0)
        }
      -- the BlocksSize event of the same GC (emitted at about the same time)
      blocksAt t = case sortOn (\(t', _) -> abs (t' - t)) (stBlocks st) of
                     ((_, b):_) -> b
                     []         -> 0
  writeFile (out ++ ".hsdat") (show res)
  writeFile (out ++ ".md") (render top res)
  putStrLn ("eps-peak: wrote " ++ out ++ ".{md,hsdat}")

step :: St -> Event -> St
step st (Event t info _) =
  let now = secs t
      st0 = st { stLast = max (stLast st) now }
  in case info of
    UserMarker m
      | Just ph <- T.stripPrefix "GHC:started: " m  -> st0 { stOpen = ph : stOpen st0 }
      | Just ph <- T.stripPrefix "GHC:finished: " m -> st0 { stOpen = deleteFirst ph (stOpen st0) }
    GCStatsGHC { gen = g } -> st0 { stLastGen = g }
    HeapLive { liveBytes = l } | stLastGen st0 < 1 ->
      st0 { stMaxLive = maxBy (now, fromIntegral l) (stMaxLive st0) }
    BlocksSize { blocksSize = b } ->
      st0 { stBlocks = (now, fromIntegral b) : stBlocks st0 }
    HeapSize { sizeBytes = b } ->
      st0 { stMaxSize = maxBy (now, fromIntegral b) (stMaxSize st0) }
    HeapLive { liveBytes = l } | stLastGen st0 >= 1 ->
      let live = fromIntegral l
          phases = map T.unpack (stOpen st0)
          mods = [ m | p <- phases, Just m <- [moduleOf p] ]
      in st0 { stMajors = (now, live, phases) : stMajors st0
             , stMaxLive = maxBy (now, live) (stMaxLive st0)
             , stPerMod = foldl' (\mp m -> Map.insertWith max m live mp) (stPerMod st0) mods }
    HeapProfSampleBegin {} -> st0 { stSample = Just (now, Map.empty) }
    HeapProfSampleString { heapProfResidency = b, heapProfLabel = lab }
      | Just (ts, m) <- stSample st0 ->
          st0 { stSample = Just (ts, Map.insertWith (+) (T.unpack lab) (fromIntegral b) m) }
    HeapProfSampleEnd {}
      | Just s <- stSample st0 -> st0 { stSample = Nothing, stSamples = s : stSamples st0 }
    _ -> st0
  where
    maxBy a@(_, x) b@(_, y) = if x >= y then a else b
    deleteFirst x (y:ys) | x == y = ys
                         | otherwise = y : deleteFirst x ys
    deleteFirst _ [] = []

-- | "Typechecker [Distribution.Parsec]" -> Just "Distribution.Parsec"
moduleOf :: String -> Maybe String
moduleOf p = case break (== '[') p of
  (_, '[':rest) -> Just (takeWhile (/= ']') rest)
  _             -> Nothing

render :: Int -> PeakResult -> String
render top r = unlines $
  [ "# Peak memory: " ++ prLabel r
  , ""
  , "| | |"
  , "|---|---|"
  , "| Peak live bytes after a major GC | " ++ fmtBytes (prPeakLive r) ++ " |"
  , "| Max live bytes over all GCs (minor GCs over-approximate) | " ++ atT (prMaxLiveAll r) ++ " |"
  , "| Largest -hT census total | " ++ atT (prMaxCensus r) ++ " |"
  , "| Max heap size | " ++ atT (prMaxHeapSize r) ++ " |"
  , "| Memory in blocks at the peak | " ++ fmtBytes (prBlocksAtPeak r) ++ " (blocks - live: " ++ fmtBytes (prBlocksAtPeak r - prPeakLive r) ++ ") |"
  , let (t, b, l) = prMaxOverhead r in
    "| Largest blocks - live at a major GC | " ++ fmtBytes (b - l) ++ printf " at %.1f s" t ++ " (blocks " ++ fmtBytes b ++ ", live " ++ fmtBytes l ++ ") |"
  , "| at | " ++ printf "%.1f s of %.1f s" (prPeakTime r) (prTotalTime r) ++ " |"
  , "| phases running (innermost first) | " ++ phasesTxt (prPeakPhases r) ++ " |"
  , "| census used | " ++ printf "%.1f s" (prCensusTime r) ++ " |"
  , ""
  , "## Biggest major GCs"
  , ""
  , "| Time | Live | Phases |"
  , "|---:|---:|---|"
  ] ++
  [ printf "| %.1f s | %s | %s |" t (fmtBytes l) (phasesTxt ps) | (t, l, ps) <- prTopGCs r ] ++
  [ ""
  , "## Max live bytes while a module was being processed"
  , ""
  , "| Module | Max live |"
  , "|---|---:|"
  ] ++
  [ "| `" ++ m ++ "` | " ++ fmtBytes b ++ " |" | (m, b) <- prPerModule r ] ++
  [ ""
  , "## Heap census (-hT) closest to the peak"
  , ""
  , "| Closure type | Bytes |"
  , "|---|---:|"
  ] ++
  [ "| `" ++ k ++ "` | " ++ fmtBytes b ++ " |" | (k, b) <- take top (prCensus r) ]
  where
    atT (t, b) = fmtBytes b ++ printf " (at %.1f s)" t
    phasesTxt [] = "(none)"
    phasesTxt ps = concatMap (\p -> "`" ++ p ++ "` ") (take 3 ps)

compareR :: FilePath -> FilePath -> Int -> IO ()
compareR fa fb top = do
  a <- read <$> readFile fa :: IO PeakResult
  b <- read <$> readFile fb :: IO PeakResult
  let ca = Map.fromList (prCensus a)
      cb = Map.fromList (prCensus b)
      keys = Map.keys (Map.union ca cb)
      deltas = sortOn (\(_, x, y) -> Down (abs (y - x)))
                 [ (k, Map.findWithDefault 0 k ca, Map.findWithDefault 0 k cb) | k <- keys ]
      phases p = case prPeakPhases p of
                   []    -> "(none)"
                   (x:_) -> "`" ++ x ++ "`"
      row name f = "| " ++ name ++ " | " ++ fmtBytes (f a) ++ " | " ++ fmtDelta (f a) (f b) ++ " |"
      rowT name f = "| " ++ name ++ " | " ++ atT (f a) ++ " | " ++ fmtDelta (snd (f a)) (snd (f b))
                      ++ printf " at %.1f s" (fst (f b)) ++ " |"
      atT (t, x) = fmtBytes x ++ printf " at %.1f s" t
  putStr $ unlines $
    [ "# Peak memory: " ++ prLabel a ++ " vs " ++ prLabel b
    , ""
    , "Percentages are relative to " ++ prLabel a ++ "."
    , ""
    , "| | " ++ prLabel a ++ " | " ++ prLabel b ++ " |"
    , "|---|---:|---:|"
    , rowT "Max live bytes, all GCs (as plotted by eventlog2html)" prMaxLiveAll
    , rowT "Max live bytes after a major GC" (\p -> (prPeakTime p, prPeakLive p))
    , rowT "Largest -hT census total" prMaxCensus
    , rowT "Max heap size" prMaxHeapSize
    , row "Memory in blocks at the peak" prBlocksAtPeak
    , row "Blocks - live at the peak" (\p -> prBlocksAtPeak p - prPeakLive p)
    , row "Largest blocks - live at a major GC" (\p -> let (_, bs, ls) = prMaxOverhead p in bs - ls)
    , "| Innermost phase at the major-GC peak | " ++ phases a ++ " | " ++ phases b ++ " |"
    , ""
    , "## Census closest to the major-GC peak: biggest differences"
    , ""
    , "| Closure type | " ++ prLabel a ++ " | " ++ prLabel b ++ " |"
    , "|---|---:|---:|"
    ] ++
    [ "| `" ++ k ++ "` | " ++ fmtBytes x ++ " | " ++ fmtDelta x y ++ " |"
    | (k, x, y) <- take top deltas ] ++
    [ row ("**Total of the top " ++ show (length (prCensus a)) ++ " types**")
          (sum . map snd . prCensus) ]

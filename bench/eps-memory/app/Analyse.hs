{-# LANGUAGE ScopedTypeVariables, TupleSections #-}
-- | eps-analyse: connect to a ghc-debug-instrumented GHC that paused itself
-- (GHC_DEBUG_PAUSE_AT_END=1, see ghc/Main.hs) after saving its
-- 'ExternalPackageState' as a ghc-debug root, and measure how much memory is
-- reachable from each field of the EPS.
--
-- Usage:
--   eps-analyse --socket SOCK --external-hs PATH/compiler/GHC/Unit/External.hs
--               --label LABEL --out PREFIX [--rts-stats FILE] [--top N]
--
-- Writes PREFIX.md (markdown tables) and PREFIX.hsdat (input for eps-compare).
module Main (main) where

import Control.Monad
import Data.Char (isAlphaNum, isSpace)
import Data.List (isPrefixOf, sortOn)
import qualified Data.Map.Strict as Map
import Data.Ord (Down(..))
import qualified Data.Text as T
import System.Environment (getArgs)
import System.Exit
import System.IO

import GHC.Debug.Client
import GHC.Debug.Trace (traceFromM, justClosures)
import GHC.Debug.Retainers (findRetainers, ClosureFilter(..))
import qualified Data.List.NonEmpty as NE
import Text.Printf (printf)
import GHC.Debug.Types.Ptr (isPinnedBlock, rawBlockAddr, rawBlockSize, applyBlockMask, blockMaxSize)
import Control.Monad.State.Strict (StateT, execStateT, modify', lift)
import GHC.Debug.Profile (closureCensusBy, closureToKey)
import GHC.Debug.Profile.Types (CensusStats(..), Count(..), Sample(..))
import Data.Semigroup (Max(..))

import Results

data Opts = Opts
  { oSocket :: FilePath, oExternal :: FilePath, oLabel :: String
  , oOut :: FilePath, oRts :: Maybe FilePath, oTop :: Int, oGhcLog :: Maybe FilePath }

parseOpts :: [String] -> Opts -> Either String Opts
parseOpts [] o = Right o
parseOpts ("--socket":v:r) o      = parseOpts r o { oSocket = v }
parseOpts ("--external-hs":v:r) o = parseOpts r o { oExternal = v }
parseOpts ("--label":v:r) o       = parseOpts r o { oLabel = v }
parseOpts ("--out":v:r) o         = parseOpts r o { oOut = v }
parseOpts ("--rts-stats":v:r) o   = parseOpts r o { oRts = Just v }
parseOpts ("--top":v:r) o         = parseOpts r o { oTop = read v }
parseOpts ("--ghc-log":v:r) o     = parseOpts r o { oGhcLog = Just v }
parseOpts (a:_) _                 = Left ("unknown argument: " ++ a)

-- | Byte arrays at least this big are reported separately: these are typically
-- whole .hi file buffers (or large slices of them).
bigArrWordsBytes :: Int
bigArrWordsBytes = 16 * 1024

main :: IO ()
main = do
  args <- getArgs
  o <- either die pure (parseOpts args (Opts "" "" "" "" Nothing 12 Nothing))
  when (any null [oSocket o, oExternal o, oLabel o, oOut o]) $
    die "usage: eps-analyse --socket SOCK --external-hs FILE --label L --out PREFIX [--rts-stats F] [--top N]"
  fields <- epsFieldNames <$> readFile (oExternal o)
  when (null fields) $ die ("could not find EPS fields in " ++ oExternal o)
  rts <- maybe (pure Map.empty) readRtsStats (oRts o)
  hPutStrLn stderr "eps-analyse: waiting for the debuggee to pause"
  (res, rets, frag0) <- withDebuggeeConnect (oSocket o) $ \e -> do
    pausePoll e
    hPutStrLn stderr "eps-analyse: debuggee paused, analysing"
    run e (analyse (oTop o) fields)
  -- ghc/Main.hs prints "ghc-debug: live_bytes=N" before pausing
  live <- case oGhcLog o of
    Nothing -> pure Nothing
    Just f  -> do
      ls <- lines <$> readFile f
      let pfx = "ghc-debug: live_bytes="
      pure $ case [ read (drop (length pfx) l) | l <- ls, pfx `isPrefixOf` l ] of
        (n:_) -> Just n
        []    -> Nothing
  writeFile (oOut o ++ "-retainers.md") (renderRetainers rets)
  let frag = frag0 { fgLabel = oLabel o }
  writeFile (oOut o ++ "-frag.hsdat") (show frag)
  writeFile (oOut o ++ "-frag.md") (renderFrag frag)
  let res' = res { rsLabel = oLabel o, rsRts = rts, rsLiveBytes = live }
  writeResults (oOut o ++ ".hsdat") res'
  writeFile (oOut o ++ ".md") (renderMarkdown res')
  hPutStrLn stderr ("eps-analyse: wrote " ++ oOut o ++ ".{md,hsdat}")

-- | Field names of the EPS constructor, in declaration order, parsed from
-- the source of GHC.Unit.External, so one binary works for every variant.
epsFieldNames :: String -> [String]
epsFieldNames src =
  [ f
  | l <- takeWhile (not . isClose) (drop 1 (dropWhile (not . isOpen) (lines src)))
  , let l' = dropWhile (\c -> isSpace c || c `elem` "{,") l
  , "eps_" `isPrefixOf` l'
  , let (f, rest) = span (\c -> isAlphaNum c || c `elem` "_'") l'
  , "::" `isPrefixOf` dropWhile isSpace rest
  ]
  where
    isOpen l  = "= EPS {" `isPrefixOf` dropWhile isSpace l
    isClose l = "}" `isPrefixOf` dropWhile isSpace l

-- | Per closure-type census plus a pseudo key for big byte arrays.
type Census = Map.Map String Stats

csToStats :: CensusStats -> Stats
csToStats cs = Stats (getCount (cscount cs)) (getSize (cssize cs))

_censusFrom :: [ClosurePtr] -> DebugM Census
_censusFrom cps = Map.map csToStats <$> closureCensusBy go cps
  where
    go cp sc = do
      d <- hextraverse pure pure pure dereferenceConDesc pure pure sc
      let key = case noSize d of
            ArrWordsClosure { bytes = b }
              | fromIntegral b >= bigArrWordsBytes -> "ARR_WORDS (big)"
            c -> T.unpack (closureToKey c)
      pure (Just (key, mkCS cp (dcSize sc)))
    mkCS cp s = CS (Count 1) s (Max s) (Sample [cp])

closureKey :: DebugClosure ccs srt pap ConstrDesc s b -> String
closureKey c = case c of
  ArrWordsClosure { bytes = b }
    | fromIntegral b >= bigArrWordsBytes -> "ARR_WORDS (big)"
  _ -> T.unpack (closureToKey c)

-- | Constructors at which 'censusStopping' does not descend: lazily
-- typechecked thunks in the EPS retain the session ('HscEnv'), and through
-- its IORef the 'EPS' itself, so without stopping every field would reach
-- everything.
isStopClosure :: DebugClosure ccs srt pap ConstrDesc s b -> Bool
isStopClosure c = case c of
  ConstrClosure { constrDesc = ConstrDesc { name = n, modl = m } } ->
    (n, m) `elem` [ ("EPS", "GHC.Unit.External")
                  , ("HscEnv", "GHC.Driver.Env.Types") ]
  _ -> False

-- | Census of everything reachable from the given closures, without entering
-- (or counting) 'isStopClosure' closures.
censusStopping :: [ClosurePtr] -> DebugM Census
censusStopping cps = execStateT (traceFromM (justClosures go) cps) Map.empty
  where
    go :: ClosurePtr -> SizedClosure -> StateT Census DebugM () -> StateT Census DebugM ()
    go _cp sc k = do
      d <- lift (hextraverse pure pure pure dereferenceConDesc pure pure sc)
      let c = noSize d
      unless (isStopClosure c) $ do
        modify' (Map.insertWith (<>) (closureKey c) (Stats 1 (getSize (dcSize sc))))
        k

totalOf :: Census -> Stats
totalOf = mconcat . Map.elems

arrWordsOf :: Census -> Stats
arrWordsOf c = Map.findWithDefault mempty "ARR_WORDS" c <> bigArrWordsOf c

bigArrWordsOf :: Census -> Stats
bigArrWordsOf = Map.findWithDefault mempty "ARR_WORDS (big)"

-- | How many retainer paths of big byte arrays to report.
retainerSamples :: Int
retainerSamples = 8

-- | Retainer paths: (EPS field, [(closure type, size)] from the array up to the field).
type Retainers = [(String, [(String, Int)])]

analyse :: Int -> [String] -> DebugM (Results, Retainers, FragResult)
analyse top fieldNames = do
  blocks <- precacheBlocks
  saved <- savedObjects
  -- Saved by ghc/Main.hs: the EPS, and (newer compilers) the HscEnv.
  -- NB: gcRoots cannot be used: after the debuggee paused itself the RTS
  -- refuses to list threads ("rts_listThreads: called from a different OS
  -- thread than rts_pause").
  (eps, extraRoots) <- case saved of
    (cp:rest) -> pure (cp, rest)
    []        -> error "no saved objects; is GHC_DEBUG_PAUSE_AT_END set?"
  clos <- dereferenceClosure eps
  ptrs <- case noSize clos of
    ConstrClosure { ptrArgs = ps, constrDesc = cd } -> do
      ConstrDesc { name = n, modl = m } <- dereferenceConDesc cd
      unless (n == "EPS") $ error ("saved object is " ++ m ++ "." ++ n ++ ", not EPS")
      pure ps
    _ -> error "saved object is not a constructor; was it evaluated?"
  labels <- if length ptrs == length fieldNames
    then pure fieldNames
    else do
      traceWrite ("warning: EPS has " ++ show (length ptrs) ++ " pointer fields but the source declares "
                  ++ show (length fieldNames) ++ " (some fields are unpacked); using positional names")
      pure ["ptr#" ++ show i | i <- [0 .. length ptrs - 1]]

  traceWrite "eps-analyse: all EPS fields"
  epsCensus <- censusStopping ptrs
  fieldResults <- forM (zip3 [0 :: Int ..] labels ptrs) $ \(i, lbl, p) -> do
    traceWrite ("eps-analyse: field " ++ lbl)
    c <- censusStopping [p]
    let total = totalOf c
    -- exclusive = all fields - all other fields; only worth computing for
    -- big fields (for small ones it is at most 'total').
    excl <- if stBytes total < 1024 * 1024
      then pure (stBytes total)
      else do
        others <- censusStopping [q | (j, q) <- zip [0 ..] ptrs, j /= i]
        pure (stBytes (totalOf epsCensus) - stBytes (totalOf others))
    pure FieldResult
      { frName = lbl
      , frReachable = total
      , frExclusive = excl
      , frArrWords = arrWordsOf c
      , frBigArrWords = bigArrWordsOf c
      , frTopTypes = take top (sortOn (Down . stBytes . snd) (Map.toList c))
      }

  traceWrite "eps-analyse: retainers of big byte arrays"
  bigRets <- findRetainers (Just retainerSamples)
               (AndFilter (InfoFilter ((== ARR_WORDS) . tipe))
                          (SizeFilter (>= Size bigArrWordsBytes)))
               ptrs
  retainerStacks <- forM bigRets $ \st -> forM (NE.toList st) $ \cp -> do
    sc <- dereferenceClosure cp
    d <- hextraverse pure pure pure dereferenceConDesc pure pure sc
    pure (closureKey (noSize d), getSize (dcSize sc))
  let fieldOf st = case [ l | (l, p) <- zip labels ptrs, p `elem` NE.toList st ] of
        (l:_) -> l
        []    -> "?"
      retainers = zip (map fieldOf bigRets) retainerStacks

  traceWrite "eps-analyse: pinned-block fragmentation"
  frag <- fragmentation blocks (eps : extraRoots)

  traceWrite "eps-analyse: everything reachable from the saved roots (EPS + HscEnv)"
  heap <- Map.map csToStats <$> closureCensusBy heapGo (eps : extraRoots)
  pure . (, retainers, frag) $ Results
    { rsLabel = ""
    , rsEpsReachable = totalOf epsCensus
    , rsEpsArrWords = arrWordsOf epsCensus
    , rsEpsBigArrWords = bigArrWordsOf epsCensus
    , rsLiveHeap = totalOf heap
    , rsEpsClosures = stCount (Map.findWithDefault mempty "EPS" heap)
    , rsFields = fieldResults
    , rsRts = Map.empty
    , rsLiveBytes = Nothing
    }
  where
    -- EPS constructors are counted under "EPS", everything else under
    -- "other"; the live heap is the sum of both.
    heapGo cp sc = do
      d <- hextraverse pure pure pure dereferenceConDesc pure pure sc
      let s = dcSize sc
          isEps = case noSize d of
            ConstrClosure { constrDesc = ConstrDesc { name = "EPS", modl = "GHC.Unit.External" } } -> True
            _ -> False
      pure (Just (if isEps then "EPS" else "other", CS (Count 1) s (Max s) (Sample [cp])))

-- | Census of the pinned block groups: which live (reachable) closures are in them.
fragmentation :: [RawBlock] -> [ClosurePtr] -> DebugM FragResult
fragmentation blocks roots = do
  let pinned = Map.fromList [ (rawBlockAddr b, rawBlockSize b) | b <- blocks, isPinnedBlock b ]
      blockBytes = fromIntegral blockMaxSize :: Int
      go cp sc = do
        -- objects start in the first block of their group
        let bp = applyBlockMask cp
            sz = getSize (dcSize sc)
        pure $ if bp `Map.member` pinned
          then Just (bp, (Stats 1 sz, [sz]))
          else Nothing
  census <- closureCensusBy go roots
  let groups = [ (sz, Map.lookup bp census) | (bp, sz) <- Map.toList pinned ]
      classify (sz, _) = if sz <= blockBytes then "1 block" else "multi-block"
      buckets = [64, 256, 1024, 4096, 65536]
      bucketOf x = case [ b | b <- buckets, x <= b ] of
                     (b:_) -> b
                     []    -> maxBound
      mkClass k gs = GroupClass
        { gcKind = k
        , gcCount = length gs
        , gcBytes = sum (map fst gs)
        , gcLive = mconcat [ st | (_, Just (st, _)) <- gs ]
        , gcHist = Map.toList $ Map.fromListWith (\(a1, b1) (a2, b2) -> (a1 + a2, b1 + b2))
            [ (min 9 ((live * 10) `div` max 1 sz), (1, sz))
            | (sz, m) <- gs, let live = maybe 0 (stBytes . fst) m ]
        , gcObjSizes = Map.toList $ Map.fromListWith (+)
            [ (bucketOf x, 1) | (_, Just (_, szs)) <- gs, x <- szs ]
        }
      byKind = Map.fromListWith (++) [ (classify g, [g]) | g <- groups ]
  pure FragResult
    { fgLabel = ""
    , fgBlocks = length blocks
    , fgBlockBytes = sum (map rawBlockSize blocks)
    , fgGroups = [ mkClass k gs | (k, gs) <- Map.toList byKind ]
    }

renderFrag :: FragResult -> String
renderFrag f = unlines $
  [ "# Pinned-block fragmentation at the end of the build: " ++ fgLabel f
  , ""
  , "Live = reachable from the EPS and the HscEnv (a lower bound of what is live)."
  , "The GC keeps a pinned block group as a whole while any object in it is live."
  , ""
  , "Blocks (groups) in the heap: " ++ show (fgBlocks f) ++ " (" ++ fmtBytes (fgBlockBytes f) ++ ")"
  , ""
  , "| Pinned groups | Count | Size | Live | Utilisation | Wasted |"
  , "|---|---:|---:|---:|---:|---:|"
  ] ++
  [ "| " ++ gcKind g ++ " | " ++ show (gcCount g) ++ " | " ++ fmtBytes (gcBytes g) ++ " | "
    ++ fmtBytes (stBytes (gcLive g)) ++ " (" ++ show (stCount (gcLive g)) ++ " objects) | "
    ++ pct (stBytes (gcLive g)) (gcBytes g) ++ " | " ++ fmtBytes (gcBytes g - stBytes (gcLive g)) ++ " |"
  | g <- fgGroups f ] ++
  concat
  [ [ ""
    , "## Utilisation of pinned groups: " ++ gcKind g
    , ""
    , "| Utilisation | Groups | Size |"
    , "|---|---:|---:|"
    ] ++
    [ "| " ++ show (d * 10) ++ "-" ++ show (d * 10 + 10) ++ "% | " ++ show n ++ " | " ++ fmtBytes b ++ " |"
    | (d, (n, b)) <- gcHist g ] ++
    [ ""
    , "Live objects by size: " ++ unwords
        [ "<=" ++ (if b == maxBound then "inf" else show b) ++ "B:" ++ show n | (b, n) <- gcObjSizes g ]
    ]
  | g <- fgGroups f ]
  where
    pct a b | b == 0 = "-"
            | otherwise = printf "%.1f%%" (100 * fromIntegral a / fromIntegral b :: Double)

renderRetainers :: Retainers -> String
renderRetainers rs = unlines $
  [ "# Retainers of big byte arrays (>= 16 KiB) reachable from the EPS", "" ] ++
  concat
  [ [ "## Sample " ++ show i ++ " (via `" ++ fld ++ "`)", ""
    , "| # | Closure | Size |", "|---:|---|---:|" ] ++
    [ "| " ++ show j ++ " | `" ++ k ++ "` | " ++ fmtBytes sz ++ " |" | (j, (k, sz)) <- zip [0 :: Int ..] st ] ++ [""]
  | (i, (fld, st)) <- zip [0 :: Int ..] rs ]

renderMarkdown :: Results -> String
renderMarkdown r = unlines $
  [ "# EPS memory: " ++ rsLabel r
  , ""
  , "| Metric | Value |"
  , "|---|---:|"
  , "| RTS live bytes at the pause (after major GC) | " ++ maybe "?" fmtBytes (rsLiveBytes r) ++ " |"
  , "| Reachable from EPS + HscEnv | " ++ fmtBytes (stBytes (rsLiveHeap r)) ++ " |"
  , "| Reachable from the EPS | " ++ fmtBytes (stBytes (rsEpsReachable r)) ++ " |"
  , "| ARR_WORDS reachable from the EPS | " ++ fmtBytes (stBytes (rsEpsArrWords r))
      ++ " (" ++ show (stCount (rsEpsArrWords r)) ++ ") |"
  , "| big ARR_WORDS (>= 16 KiB) reachable from the EPS | " ++ fmtBytes (stBytes (rsEpsBigArrWords r))
      ++ " (" ++ show (stCount (rsEpsBigArrWords r)) ++ ") |"
  , "| EPS constructors reachable from EPS + HscEnv | " ++ show (rsEpsClosures r) ++ " |"
  ] ++
  [ "| RTS " ++ k ++ " | " ++ v ++ " |"
  | k <- ["max_bytes_used", "max_mem_in_use_bytes", "bytes allocated", "total_wall_seconds"]
  , Just v <- [Map.lookup k (rsRts r)] ] ++
  [ ""
  , "## Per EPS field"
  , ""
  , "Reachable includes structure shared with other fields; exclusive is reachable from this field only."
  , ""
  , "| Field | Reachable | Closures | Exclusive | ARR_WORDS | big ARR_WORDS |"
  , "|---|---:|---:|---:|---:|---:|"
  ] ++
  [ "| `" ++ frName f ++ "` | " ++ fmtBytes (stBytes (frReachable f)) ++ " | "
    ++ show (stCount (frReachable f)) ++ " | " ++ fmtBytes (frExclusive f) ++ " | "
    ++ fmtBytes (stBytes (frArrWords f)) ++ " | "
    ++ fmtBytes (stBytes (frBigArrWords f)) ++ " (" ++ show (stCount (frBigArrWords f)) ++ ") |"
  | f <- sortOn (Down . stBytes . frReachable) (rsFields r) ] ++
  concat
  [ [ ""
    , "### Top closure types reachable from `" ++ frName f ++ "`"
    , ""
    , "| Closure type | Bytes | Count |"
    , "|---|---:|---:|"
    ] ++
    [ "| `" ++ k ++ "` | " ++ fmtBytes (stBytes s) ++ " | " ++ show (stCount s) ++ " |"
    | (k, s) <- frTopTypes f ]
  | f <- sortOn (Down . stBytes . frReachable) (rsFields r)
  , stBytes (frReachable f) >= 1024 * 1024 ]

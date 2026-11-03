-- | eps-compare: combine eps-analyse results of several compiler variants
-- into one markdown table. The first file is the baseline.
--
-- Usage: eps-compare BASE.hsdat NEW.hsdat [MORE.hsdat ...] > comparison.md
module Main (main) where

import Data.List (nub)
import qualified Data.Map.Strict as Map
import System.Environment (getArgs)
import System.Exit (die)

import Results

main :: IO ()
main = do
  files <- getArgs
  rs <- mapM readResults files
  case rs of
    (base : rest@(_:_)) -> putStr (render base rest)
    _ -> die "usage: eps-compare BASE.hsdat NEW.hsdat [MORE.hsdat ...]"

render :: Results -> [Results] -> String
render base rest = unlines $
  [ "# EPS memory comparison (baseline: " ++ rsLabel base ++ ")"
  , ""
  , header "Metric"
  , sep
  ] ++
  [ row name (f base) [f r | r <- rest] | (name, f) <- summary ] ++
  [ ""
  , "## Bytes reachable per EPS field"
  , ""
  , "Fields missing in a variant are shown as `-`. Reachable sizes include shared structure."
  , ""
  , header "Field"
  , sep
  ] ++
  [ fieldRow fld | fld <- allFields ] ++
  [ ""
  , "## Exclusive bytes per EPS field"
  , ""
  , header "Field"
  , sep
  ] ++
  [ fieldRowWith frExclusive fld | fld <- allFields ]
  where
    allRs = base : rest
    header h = "| " ++ h ++ " | " ++ rsLabel base ++ concatMap (\r -> " | " ++ rsLabel r) rest ++ " |"
    sep = "|---|---:" ++ concatMap (const "|---:") rest ++ "|"

    summary =
      [ ("RTS live bytes at the pause", maybe 0 id . rsLiveBytes)
      , ("Reachable from EPS + HscEnv", stBytes . rsLiveHeap)
      , ("Reachable from the EPS", stBytes . rsEpsReachable)
      , ("ARR_WORDS from the EPS", stBytes . rsEpsArrWords)
      , ("big ARR_WORDS (>= 16 KiB) from the EPS", stBytes . rsEpsBigArrWords)
      , ("RTS max_bytes_used", rtsInt "max_bytes_used")
      , ("RTS max_mem_in_use_bytes", rtsInt "max_mem_in_use_bytes")
      , ("RTS bytes allocated", rtsInt "bytes allocated")
      ]
    rtsInt k r = maybe 0 read (Map.lookup k (rsRts r))

    row name b ns = "| " ++ name ++ " | " ++ fmtBytes b ++ concatMap (\n -> " | " ++ fmtDelta b n) ns ++ " |"

    allFields = nub (concatMap (map frName . rsFields) allRs)
    lookupField fld r = [f | f <- rsFields r, frName f == fld]

    fieldRow = fieldRowWith (stBytes . frReachable)
    fieldRowWith sel fld =
      let cell r = case lookupField fld r of
            (f:_) -> Just (sel f)
            []    -> Nothing
          b = cell base
          showCell (Just v) = case b of
            Just bv -> fmtDelta bv v
            Nothing -> fmtBytes v
          showCell Nothing = "-"
      in "| `" ++ fld ++ "` | " ++ maybe "-" fmtBytes b
           ++ concatMap (\r -> " | " ++ showCell (cell r)) rest ++ " |"

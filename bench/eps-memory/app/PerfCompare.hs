-- | perf-compare: compare the RTS stats of perf-test runs made by
-- perf/run-perf-tests.sh.
--
-- Usage:
--   perf-compare [--metrics m1,m2,..] BASE_DIR NEW_DIR [NEW_DIR ..]
--
-- Each directory is results/perf/LABEL with subdirectories repN holding
-- <test>.<suffix>.stats files (GHC's +RTS -t --machine-readable output).
-- For every metric, prints a markdown table: one row per stats file, the
-- median over the repetitions for each label, and for every label but the
-- first the change relative to the first. The last row is the geometric
-- mean of the ratios.
module Main (main) where

import Control.Monad (forM, filterM)
import Data.List (isSuffixOf, sort, intercalate, dropWhileEnd)
import Data.Maybe (mapMaybe)
import qualified Data.Map.Strict as M
import System.Directory
import System.Environment (getArgs)
import System.Exit (die)
import System.FilePath
import Text.Printf (printf)

-- | stats file name -> metric -> values over the repetitions
type Runs = M.Map String (M.Map String [Double])

data Metric = Metric
  { mKey   :: String            -- ^ key in the stats file, or a derived one
  , mTitle :: String
  , mFmt   :: Double -> String
  }

metrics :: [Metric]
metrics =
  [ Metric "bytes allocated"          "Bytes allocated"                 fmtBytes
  , Metric "total_cpu_seconds"        "CPU time (mutator + GC), seconds" fmtSecs
  , Metric "mut_cpu_seconds"      "Mutator CPU time, seconds"       fmtSecs
  , Metric "GC_cpu_seconds"           "GC CPU time, seconds"            fmtSecs
  , Metric "max_bytes_used"           "max_bytes_used"                  fmtBytes
  , Metric "peak_megabytes_allocated" "peak_megabytes_allocated (MiB)"  (printf "%.0f")
  ]

main :: IO ()
main = do
  args <- getArgs
  (sel, dirs) <- case args of
    ("--metrics" : ms : rest) -> pure (Just (splitOn ',' ms), rest)
    _                         -> pure (Nothing, args)
  case dirs of
    (_ : _ : _) -> pure ()
    _ -> die "usage: perf-compare [--metrics m1,m2,..] BASE_DIR NEW_DIR [NEW_DIR ..]"
  runs <- mapM loadRuns dirs
  let labels = map (takeFileName . dropTrailingPathSeparator) dirs
      chosen = maybe metrics (\ks -> [m | m <- metrics, mKey m `elem` ks]) sel
  mapM_ (printMetric labels runs) chosen

splitOn :: Char -> String -> [String]
splitOn c s = case break (== c) s of
  (a, [])     -> [a]
  (a, _ : r)  -> a : splitOn c r

loadRuns :: FilePath -> IO Runs
loadRuns dir = do
  entries <- listDirectory dir
  reps <- filterM (doesDirectoryExist . (dir </>)) [e | e <- entries, take 3 e == "rep"]
  perRep <- forM reps $ \r -> do
    fs <- filter (".stats" `isSuffixOf`) <$> listDirectory (dir </> r)
    forM fs $ \f -> do
      s <- readFile (dir </> r </> f)
      pure (dropExtension f, parseStats s)
  pure $ M.unionsWith (M.unionWith (++))
    [ M.singleton name (M.map pure kvs) | rep <- perRep, (name, kvs) <- rep ]

-- | The file is the command line followed by a Haskell list of pairs.
parseStats :: String -> M.Map String Double
parseStats s =
  case reads (dropWhile (/= '[') s) :: [([(String, String)], String)] of
    ((kvs, _) : _) ->
      let m = M.fromList (mapMaybe num kvs)
          total = (+) <$> M.lookup "mut_cpu_seconds" m <*> M.lookup "GC_cpu_seconds" m
      in maybe m (\t -> M.insert "total_cpu_seconds" t m) total
    [] -> M.empty
  where
    num (k, v) = case reads v of
      [(d, "")] -> Just (k, d)
      _         -> Nothing

median :: [Double] -> Double
median xs = let ys = sort xs; n = length ys
            in if even n then (ys !! (n `div` 2 - 1) + ys !! (n `div` 2)) / 2
                         else ys !! (n `div` 2)

printMetric :: [String] -> [Runs] -> Metric -> IO ()
printMetric labels runs m = do
  let base = head runs
      tests = M.keys base
      val r t = median <$> (M.lookup t r >>= M.lookup (mKey m))
      rows = [ (t, map (`val` t) runs) | t <- tests, Just _ <- [val base t] ]
  if null rows then pure () else do
    putStrLn ("## " ++ mTitle m)
    putStrLn ""
    putStrLn ("| Test | " ++ intercalate " | " labels ++ " |")
    putStrLn ("|---|" ++ concatMap (const "---:|") labels)
    mapM_ (\(t, vs) -> putStrLn ("| " ++ t ++ " | " ++ intercalate " | " (cells vs) ++ " |")) rows
    let geo i = let rs = [ n / b | (_, vs) <- rows, Just b <- [head vs], Just n <- [vs !! i], b > 0, n > 0 ]
                in if null rs then "-" else pct (exp (sum (map log rs) / fromIntegral (length rs)))
    putStrLn ("| **geo. mean** | | " ++ intercalate " | " (map geo [1 .. length labels - 1]) ++ " |")
    putStrLn ""
  where
    cells (b : ns) = maybe "-" (mFmt m) b : map (cell b) ns
    cells [] = []
    cell (Just b) (Just n) | b > 0 = mFmt m n ++ " (" ++ pct (n / b) ++ ")"
    cell _ (Just n) = mFmt m n
    cell _ Nothing = "-"

pct :: Double -> String
pct r = let p = (r - 1) * 100 in (if p >= 0 then "+" else "") ++ printf "%.1f%%" p

fmtBytes :: Double -> String
fmtBytes b
  | b >= 2 ^ (20 :: Int) = printf "%.1f MiB" (b / 2 ^ (20 :: Int))
  | b >= 2 ^ (10 :: Int) = printf "%.1f KiB" (b / 2 ^ (10 :: Int))
  | otherwise            = printf "%.0f B" b

fmtSecs :: Double -> String
fmtSecs = dropWhileEnd (== ' ') . printf "%.3f"

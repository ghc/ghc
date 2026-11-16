-- | eps-hackage: preparation and reporting for the EPS benchmark. Built with
-- the system GHC; drives cabal with the compiler under test.
--
-- > eps-hackage deps    --ghc GHC --project DIR --work DIR [--jobs N]
-- > eps-hackage flags   --ghc GHC --project DIR --work DIR [--opt -O1]
-- > eps-hackage summary RESULT-FILE...
--
-- deps   builds the dependencies of the benchmark package (pandoc's library)
--        with GHC, into WORK/store (one cabal store per compiler: they all
--        report the same version, but their libraries differ). Fetches the
--        package into the project directory first, if needed.
-- flags  records the GHC arguments cabal uses for the package: it runs
--        `cabal repl` with this program as the REPL (--with-repl), which
--        writes its arguments instead of starting a REPL. Writes
--        WORK/ghc.rsp (a GHC response file, without output directories and
--        with the optimisation level OPT) and WORK/ghc.rsp.cwd.
-- summary prints a markdown table of results of eps-hackage-run; the first
--        one is the baseline. Also compares the output hashes of the runs
--        of each label (if recorded).
module Main (main) where

import EpsHackage.Result

import Control.Monad
import Data.List ( intercalate, isPrefixOf )
import qualified Data.Map.Strict as M
import GHC.ResponseFile ( escapeArgs )
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import System.Process
import Text.Printf ( printf )

-- | The benchmark package and component.
package, component :: String
package   = "pandoc-3.14"
component = "lib:pandoc"

-- | Set when cabal runs us as its REPL.
recordVar :: String
recordVar = "EPS_HACKAGE_RECORD"

main :: IO ()
main = do
  record <- lookupEnv recordVar
  args <- getArgs
  case record of
    Just out -> do
      -- Run by `cabal repl --with-repl`: record, don't start a REPL.
      cwd <- getCurrentDirectory
      writeFile out (show (cwd, args))
    Nothing -> case args of
      "deps" : r    -> withOpts r deps
      "flags" : r   -> withOpts r flags
      "summary" : r -> summary r
      _ -> die usage

usage :: String
usage = unlines
  [ "usage: eps-hackage deps  --ghc GHC --project DIR --work DIR [--jobs N]"
  , "       eps-hackage flags --ghc GHC --project DIR --work DIR [--opt -O1]"
  , "       eps-hackage summary RESULT-FILE..." ]

data Opts = Opts
  { oGhc, oProject, oWork, oOpt :: FilePath
  , oJobs :: Int }

withOpts :: [String] -> (Opts -> IO ()) -> IO ()
withOpts args k = do
  o <- either die pure (go args (Opts "" "" "" "-O1" 12))
  when (any null [oGhc o, oProject o, oWork o]) $ die usage
  ghc  <- makeAbsolute (oGhc o)
  proj <- makeAbsolute (oProject o)
  work <- makeAbsolute (oWork o)
  createDirectoryIfMissing True work
  k o { oGhc = ghc, oProject = proj, oWork = work }
  where
    go a o = case a of
      "--ghc" : v : r     -> go r o { oGhc = v }
      "--project" : v : r -> go r o { oProject = v }
      "--work" : v : r    -> go r o { oWork = v }
      "--opt" : v : r     -> go r o { oOpt = v }
      "--jobs" : v : r    -> go r o { oJobs = read v }
      x : _               -> Left ("unknown argument " ++ x)
      []                  -> Right o

-- | cabal, with the compiler, store and build directory of this compiler.
cabal :: Opts -> String -> [String] -> CreateProcess
cabal o cmd extra = (proc "cabal" (
  [ "--store-dir=" ++ oWork o </> "store", cmd
  , "--project-dir=" ++ oProject o
  , "--builddir=" ++ oWork o </> "dist"
  , "--with-compiler=" ++ oGhc o
  , "--with-hc-pkg=" ++ takeDirectory (oGhc o) </> "ghc-pkg"
  ] ++ extra)) { cwd = Just (oProject o) }

-- | Run a process with its output going to a log file; on failure show the
-- end of the log and exit.
runLogged :: FilePath -> CreateProcess -> IO ()
runLogged logFile p = do
  ec <- withFile logFile WriteMode $ \h -> do
    (_, _, _, ph) <- createProcess p { std_out = UseHandle h, std_err = UseHandle h }
    waitForProcess ph
  case ec of
    ExitSuccess -> pure ()
    _ -> do
      out <- lines <$> readFile logFile
      hPutStr stderr (unlines (drop (length out - 30) out))
      die ("failed, see " ++ logFile)

deps :: Opts -> IO ()
deps o = do
  have <- doesDirectoryExist (oProject o </> package)
  unless have $ do
    putStrLn ("== fetching " ++ package)
    callProcess "cabal" ["get", "-v0", "--destdir=" ++ oProject o, package]
  putStrLn ("== building the dependencies of " ++ component ++ " with " ++ oGhc o)
  runLogged (oWork o </> "deps.log") $
    cabal o "build" ["-j" ++ show (oJobs o), "--only-dependencies", component]

flags :: Opts -> IO ()
flags o = do
  self <- getExecutablePath
  let recordFile = oWork o </> "ghc.args"
  removePathForcibly recordFile
  env <- getEnvironment
  putStrLn ("== recording the GHC arguments of " ++ component)
  runLogged (oWork o </> "flags.log") $
    (cabal o "repl" ["--with-repl=" ++ self, component])
      { env = Just ((recordVar, recordFile) : env) }
  (cwd, args) <- read <$> readFile recordFile :: IO (FilePath, [String])
  let args' = oOpt o : rewrite args
  writeFile (oWork o </> "ghc.rsp") (escapeArgs args')
  writeFile (oWork o </> "ghc.rsp.cwd") cwd
  printf "== %d arguments in %s\n" (length args') (oWork o </> "ghc.rsp")
  where
    -- Drop what is specific to the REPL and the output locations (the
    -- runner sets those), and the optimisation level (we add OPT).
    rewrite (f : _ : r) | f `elem` dirFlags = rewrite r
    rewrite (f : r)
      | f `elem` ["--interactive", "-O0", "-O", "-O1", "-O2", "-fbyte-code", "-fobject-code"]
      || "-j" `isPrefixOf` f = rewrite r
    rewrite (f : r) = f : rewrite r
    rewrite [] = []
    dirFlags = ["-outputdir", "-odir", "-hidir", "-hiedir", "-stubdir", "-dumpdir", "-o"]

------------------------------------------------------------------------------
-- Summary

summary :: [FilePath] -> IO ()
summary files = do
  when (null files) $ die usage
  rs <- mapM (fmap read . readFile) files :: IO [Result]
  base <- case rs of
    r : _ -> pure r
    []    -> die usage
  let
      cols :: [(String, Result -> Maybe Double)]
      cols =
        [ ("wall (s)",            Just . resWallSeconds)
        , ("mutator (s)",         Just . resMutatorSeconds)
        , ("GC (s)",              Just . resGcSeconds)
        , ("allocated (GiB)",     Just . gib . resAllocatedBytes)
        , ("max residency (MiB)", Just . mib . resMaxLiveBytes)
        , ("RTS peak (MiB)",      Just . mib . resMaxMemInUseBytes)
        , ("max RSS (MiB)",       Just . mib . resMaxRssBytes)
        , ("live at end (MiB)",   Just . mib . resLiveAtEndBytes)
        , ("rehydrations",        fmap (fromIntegral . ssRehydrations) . resStoreStats)
        ]
      cell isBase r (_, f) = case (f r, f base) of
        (Nothing, _) -> ""
        (Just v, Just b) | not isBase && b /= 0 ->
          printf "%.1f (%+.1f%%)" v ((v / b - 1) * 100)
        (Just v, _) -> printf "%.1f" v
  putStrLn ("| run | " ++ intercalate " | " (map fst cols) ++ " |")
  putStrLn ("|---|" ++ concat (replicate (length cols) "---:|"))
  forM_ (zip [0 :: Int ..] rs) $ \(i, r) ->
    putStrLn ("| " ++ resLabel r ++ "/" ++ resMode r ++ (if resSucceeded r then "" else " (FAILED)")
              ++ " | " ++ intercalate " | " (map (cell (i == 0) r) cols) ++ " |")
  -- Output hashes: runs of the same label must produce the same files.
  let byLabel = M.fromListWith (flip (++))
                  [ (resLabel r, [r]) | r <- rs, not (null (resOutputs r)) ]
  forM_ [ (ref, others) | ref : others <- M.elems byLabel ] $ \(ref, others) ->
    forM_ others $ \r -> do
      let diffs = [ f | (f, h) <- resOutputs r, lookup f (resOutputs ref) /= Just h ]
      putStrLn $ "\n" ++ resLabel r ++ "/" ++ resMode r ++ ": " ++
        if null diffs && length (resOutputs r) == length (resOutputs ref)
          then "outputs identical to " ++ resMode ref
          else show (length diffs) ++ " outputs differ from " ++ resMode ref
  where
    mib x = fromIntegral x / 2 ^ (20 :: Int) :: Double
    gib x = fromIntegral x / 2 ^ (30 :: Int) :: Double

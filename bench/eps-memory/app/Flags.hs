-- | eps-flags: use hie-bios to compute the GHC invocation that compiles
-- Cabal-syntax with a given compiler, and write it as a response file.
--
-- Usage:
--   eps-flags --ghc GHC --package-dir DIR --work DIR --out RSP [--opt -O1]
--
-- To avoid touching the cabal checkout, a cradle directory is created in
-- WORK, holding a symlink to the package, an hie.yaml and a project file
-- pinning the compiler. Writes RSP (one GHC argument per line) and
-- RSP.cwd (the directory GHC must be run from).
module Main (main) where

import Control.Monad (unless, when)
import Data.List (isPrefixOf)
import System.Directory
import System.Environment (getArgs)
import System.Exit
import System.FilePath
import System.IO
import System.Process

data Opts = Opts
  { oGhc    :: FilePath
  , oPkgDir :: FilePath
  , oWork   :: FilePath
  , oOut    :: FilePath
  , oOpt    :: String
  , oTarget :: FilePath  -- ^ a source file of the component, relative to the package dir
  }

parseOpts :: [String] -> Opts -> Either String Opts
parseOpts [] o = Right o
parseOpts ("--ghc":v:r) o         = parseOpts r o { oGhc = v }
parseOpts ("--package-dir":v:r) o = parseOpts r o { oPkgDir = v }
parseOpts ("--work":v:r) o        = parseOpts r o { oWork = v }
parseOpts ("--out":v:r) o         = parseOpts r o { oOut = v }
parseOpts ("--opt":v:r) o         = parseOpts r o { oOpt = v }
parseOpts ("--target":v:r) o      = parseOpts r o { oTarget = v }
parseOpts (a:_) _                 = Left ("unknown argument: " ++ a)

defaults :: Opts
defaults = Opts "" "" "" "" "-O1" "src/Distribution/Parsec.hs"

main :: IO ()
main = do
  args <- getArgs
  o0 <- either die pure (parseOpts args defaults)
  when (any null [oGhc o0, oPkgDir o0, oWork o0, oOut o0]) $
    die "usage: eps-flags --ghc GHC --package-dir DIR --work DIR --out RSP [--opt -O1] [--target FILE]"
  ghc    <- makeAbsolute (oGhc o0)
  pkgDir <- makeAbsolute (oPkgDir o0)
  work   <- makeAbsolute (oWork o0)
  let o = o0 { oGhc = ghc, oPkgDir = pkgDir, oWork = work }
      pkgName = takeFileName (dropTrailingPathSeparator pkgDir)
      cradle  = work </> "cradle"
      link    = cradle </> pkgName
      ghcPkg  = takeDirectory ghc </> "ghc-pkg"
  createDirectoryIfMissing True cradle
  linkExists <- doesPathExist link
  unless linkExists $ createDirectoryLink pkgDir link
  writeFile (cradle </> "cabal.project.eps-bench") $ unlines
    [ "packages: " ++ pkgName
    , "with-compiler: " ++ ghc
    , "with-hc-pkg: " ++ ghcPkg
    ]
  writeFile (cradle </> "hie.yaml") $ unlines
    [ "cradle:"
    , "  cabal:"
    , "    cabalProject: cabal.project.eps-bench"
    , "    component: \"lib:" ++ pkgName ++ "\""
    ]
  hPutStrLn stderr ("eps-flags: running hie-bios in " ++ cradle)
  (ec, out, err) <- readCreateProcessWithExitCode
    ((proc "hie-bios" ["flags", pkgName </> oTarget o]) { cwd = Just cradle }) ""
  case ec of
    ExitSuccess -> pure ()
    _ -> hPutStr stderr (out ++ err) >> die "eps-flags: hie-bios failed"
  (cwdDir, flags) <- either die pure (parseHieBios out)
  let flags' = rewriteFlags (oOpt o) flags
  writeFile (oOut o) (unlines flags')
  writeFile (oOut o <.> "cwd") cwdDir
  hPutStrLn stderr ("eps-flags: wrote " ++ show (length flags') ++ " arguments to " ++ oOut o)

-- | hie-bios prints @Options: [..]@, @ComponentDir: ..@ and @Dependencies: [..]@.
parseHieBios :: String -> Either String (FilePath, [String])
parseHieBios out = do
  let ls = lines out
      field p = [drop (length p) l | l <- ls, p `isPrefixOf` l]
  optsStr <- case field "Options: " of
    (s:_) -> Right s
    []    -> Left ("no Options in hie-bios output:\n" ++ out)
  flags <- case reads optsStr of
    [(fs, _)] -> Right fs
    _         -> Left "could not parse Options list"
  dir <- case field "ComponentDir: " of
    (d:_) -> Right d
    []    -> Left "no ComponentDir in hie-bios output"
  pure (dir, flags)

-- | Drop the output-directory flags (run-bench passes its own -outputdir)
-- and the optimisation level chosen for a REPL; keep everything else.
rewriteFlags :: String -> [String] -> [String]
rewriteFlags opt = (opt :) . go
  where
    dirFlags = ["-outputdir", "-odir", "-hidir", "-hiedir", "-stubdir"]
    go (f:_:r) | f `elem` dirFlags = go r
    go (f:r)   | f `elem` ["-O0", "-O", "-O1", "-O2"] = go r
    go (f:r) = f : go r
    go [] = []

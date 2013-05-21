#!/usr/bin/env runhaskell
\begin{code}
{-# LANGUAGE CPP #-}
import Prelude hiding (mod)
import Control.Monad
import Control.Applicative
import Data.List
import Data.Maybe
import Distribution.InstalledPackageInfo
import Distribution.Package (PackageName (..))
import Distribution.Simple.Compiler
import Distribution.Simple.GHC
import Distribution.Simple.PackageIndex
import Distribution.Simple.Program
import Distribution.Simple.Utils
import Distribution.Verbosity
import System.IO
import System.Cmd
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.Process (ProcessHandle, runProcess, waitForProcess)


packageRoot, dataDir, haddockPath, baseDir, testDir, outDir :: FilePath
baseDir = takeDirectory __FILE__
testDir       = baseDir </> "src"
refDir        = baseDir </> "ref"
outDir        = baseDir </> "out"
packageRoot   = baseDir </> ".."
dataDir       = packageRoot </> "resources"
haddockPath   = packageRoot </> "dist" </> "build" </> "haddock" </> "haddock"


main :: IO ()
main = do
  test
  putStrLn "All tests passed!"


test :: IO ()
test = do
  x <- doesFileExist haddockPath
  unless x $ die "you need to run 'cabal build' successfully first"

  contents <- getDirectoryContents testDir
  args <- getArgs
  let (opts, spec) = span ("-" `isPrefixOf`) args
  let mods =
        case spec of
          y:_ | y /= "all" -> [y ++ ".hs"]
          _ -> filter ((==) ".hs" . takeExtension) contents

  let mods' = map (testDir </>) mods

  -- add haddock_datadir to environment for subprocesses
  env <- Just . (:) ("haddock_datadir", dataDir) <$> getEnvironment

  putStrLn ""
  putStrLn "Haddock version: "
  h1 <- runProcess haddockPath ["--version"] Nothing
                   env Nothing Nothing Nothing
  wait h1 "*** Running `haddock --version' failed!"
  putStrLn ""
  putStrLn "GHC version: "
  h2 <- runProcess haddockPath ["--ghc-version"] Nothing
                   env Nothing Nothing Nothing
  wait h2 "*** Running `haddock --ghc-version' failed!"
  putStrLn ""

  -- TODO: maybe do something more clever here using haddock.cabal
  ghcPath <- fmap init $ rawSystemStdout normal haddockPath ["--print-ghc-path"]
  (_, _, conf) <- configure normal (Just ghcPath) Nothing defaultProgramConfiguration
  pkgIndex <- getInstalledPackages normal [GlobalPackageDB] conf
  let mkDep pkgName =
        fromMaybe (error "Couldn't find test dependencies") $ do
          let pkgs = lookupPackageName pkgIndex (PackageName pkgName)
          (_, pkgs') <- listToMaybe pkgs
          pkg <- listToMaybe pkgs'
          ifacePath <- listToMaybe (haddockInterfaces pkg)
          htmlPath <- listToMaybe (haddockHTMLs pkg)
          return ("-i " ++ htmlPath ++ "," ++ ifacePath)

  let base    = mkDep "base"
      process = mkDep "process"
      ghcprim = mkDep "ghc-prim"

  putStrLn "Running tests..."
  handle <- runProcess haddockPath
                       (["-w", "-o", outDir, "-h", "--pretty-html", "--optghc=-fglasgow-exts"
                        , "--optghc=-w", base, process, ghcprim] ++ opts ++ mods')
                       Nothing env Nothing
                       Nothing Nothing

  wait handle "*** Haddock run failed! Exiting."
  check mods (if not (null args) && args !! 0 == "all" then False else True)
  where
    wait :: ProcessHandle -> String -> IO ()
    wait h msg = do
      r <- waitForProcess h
      unless (r == ExitSuccess) $ do
        hPutStrLn stderr msg
        exitFailure

check :: [FilePath] -> Bool -> IO ()
check modules strict = do
  forM_ modules $ \mod -> do
    let outfile = outDir </> dropExtension mod ++ ".html"
    let reffile = refDir </> dropExtension mod ++ ".html"
    b <- doesFileExist reffile
    if b
      then do
        out <- readFile outfile
        ref <- readFile reffile
        if not $ haddockEq out ref
          then do
            putStrLn $ "Output for " ++ mod ++ " has changed! Exiting with diff:"
            let ref' = stripLinks ref
                out' = stripLinks out
            let reffile' = outDir </> takeFileName reffile ++ ".nolinks"
                outfile' = outDir </> takeFileName outfile ++ ".ref.nolinks"
            writeFile reffile' ref'
            writeFile outfile' out'
            r <- programOnPath "colordiff"
            code <- if r
              then system $ "colordiff " ++ reffile' ++ " " ++ outfile'
              else system $ "diff " ++ reffile' ++ " " ++ outfile'
            if strict then exitFailure else return ()
            unless (code == ExitSuccess) $ do
              hPutStrLn stderr "*** Running diff failed!"
              exitFailure
          else do
            putStrLn $ "Pass: " ++ mod
      else do
        putStrLn $ "Pass: " ++ mod ++ " (no .ref file)"


haddockEq :: String -> String -> Bool
haddockEq file1 file2 = stripLinks file1 == stripLinks file2

stripLinks :: String -> String
stripLinks str =
  let prefix = "<a href=\"" in
  case stripPrefix prefix str of
    Just str' -> prefix ++ stripLinks (dropWhile (/= '"') str')
    Nothing ->
      case str of
        [] -> []
        x : xs -> x : stripLinks xs

programOnPath :: FilePath -> IO Bool
programOnPath p = do
  result <- findProgramLocation silent p
  return (isJust result)
\end{code}

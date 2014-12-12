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
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.Process (ProcessHandle, runProcess, waitForProcess, system)


packageRoot, dataDir, haddockPath, baseDir, testDir, outDir, refDir :: FilePath
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
  unless x $ System.Exit.die "you need to run 'cabal build' successfully first"

  contents <- getDirectoryContents testDir

  args <- getArgs
  let (opts, spec) = span ("-" `isPrefixOf`) args
      isDir x' = liftM2 (&&) (doesDirectoryExist $ testDir </> x')
                             (return $ x' /= "." && x' /= "..")
  modDirs <- case spec of
    y:_ | y /= "all" -> return [y]
    _ -> filterM isDir contents

  let modDirs' = map (testDir </>) modDirs

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

  forM_ modDirs' $ \modDir -> do
    testModules <- getDirectoryContents modDir

    let mods = filter ((==) ".hs" . takeExtension) testModules
        mods' = map (modDir </>) mods

    unless (null mods') $ do
      handle <- runProcess haddockPath
                (["-w", "-o", outDir </> last (splitPath modDir), "--latex"
                 , "--optghc=-fglasgow-exts"
                 , "--optghc=-w", base, process, ghcprim] ++ opts ++ mods')
                Nothing env Nothing
                Nothing Nothing

      wait handle "*** Haddock run failed! Exiting."

  check modDirs (if not (null args) && args !! 0 == "all" then False else True)
  where
    wait :: ProcessHandle -> String -> IO ()
    wait h msg = do
      r <- waitForProcess h
      unless (r == ExitSuccess) $ do
        hPutStrLn stderr msg
        exitFailure

check :: [FilePath] -> Bool -> IO ()
check modDirs strict = do
  forM_ modDirs $ \modDir -> do
    let oDir = outDir </> modDir
        rDir = refDir </> modDir

    refDirExists <- doesDirectoryExist rDir
    when refDirExists $ do
      -- we're not creating sub-directories, I think.
      refFiles <- getDirectoryContents rDir >>= filterM doesFileExist

      forM_ refFiles $ \rFile -> do
        let refFile = rDir </> rFile
            outFile = oDir </> rFile
        oe <- doesFileExist outFile
        if oe
          then do
            out <- readFile outFile
            ref <- readFile refFile

            if out /= ref
               then do
                 putStrLn $ "Output for " ++ modDir ++ " has changed! Exiting with diff:"

                 let reffile' = outDir </> takeFileName refFile ++ ".nolinks"
                     outfile' = outDir </> takeFileName outFile ++ ".ref.nolinks"
                 writeFile reffile' ref
                 writeFile outfile' out
                 r <- programOnPath "colordiff"
                 code <- if r
                   then system $ "colordiff " ++ reffile' ++ " " ++ outfile'
                   else system $ "diff " ++ reffile' ++ " " ++ outfile'
                 if strict then exitFailure else return ()
                 unless (code == ExitSuccess) $ do
                   hPutStrLn stderr "*** Running diff failed!"
                   exitFailure
               else do
                 putStrLn $ "Pass: " ++ modDir
           else do
             putStrLn $ "Pass: " ++ modDir ++ " (no .ref file)"

programOnPath :: FilePath -> IO Bool
programOnPath p = do
  result <- findProgramLocation silent p
  return (isJust result)
\end{code}

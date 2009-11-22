import System.Cmd
import System.Environment
import System.FilePath
import System.Exit
import System.Directory
import System.Process
import Data.List
import Control.Monad
import Text.Printf
import Text.Regex
import Distribution.Simple.Utils
import Distribution.Simple.Program
import Distribution.Verbosity
import Data.Maybe


main = do
  test
  putStrLn "All tests passed!"


haddockEq file1 file2 = stripLinks file1 == stripLinks file2

stripLinks f = subRegex (mkRegexWithOpts "<A HREF=[^>]*>" False False) f "<A HREF=\"\">"

programOnPath p = do
  result <- findProgramOnPath silent p
  return (isJust result)


check modules strict = do
  forM_ modules $ \mod -> do
    let outfile = "output" </> (dropExtension mod ++ ".html")
    let reffile = "tests" </> dropExtension mod ++ ".html.ref"
    b <- doesFileExist reffile
    if b
      then do
        copyFile reffile ("output" </> takeFileName reffile)
        out <- readFile outfile
        ref <- readFile reffile
        if not $ haddockEq out ref
          then do
            putStrLn $ "Output for " ++ mod ++ " has changed! Exiting with diff:"
            let ref' = stripLinks ref
                out' = stripLinks out
            let reffile' = "output" </> takeFileName reffile ++ ".nolinks"
                outfile' = "output" </> takeFileName outfile ++ ".nolinks"
            writeFile reffile' ref'
            writeFile outfile' out'
            b <- programOnPath "colordiff"
            if b
              then system $ "colordiff " ++ reffile' ++ " " ++ outfile'
              else system $ "diff " ++ reffile' ++ " " ++ outfile'
            if strict then exitFailure else return ()
          else do
            putStrLn $ "Pass: " ++ mod
      else do
        putStrLn $ "Pass: " ++ mod ++ " (no .ref file)"


test = do
  contents <- getDirectoryContents "tests"
  args <- getArgs
  let mods = filter ((==) ".hs" . takeExtension) contents
  let outdir = "output"
  let mods' = map ("tests" </>) mods
  putStrLn ""
  putStrLn "Haddock version: "
  h1 <- runProcess "../dist/build/haddock/haddock" ["--version"] Nothing (Just [("haddock_datadir", "../.")]) Nothing Nothing Nothing
  waitForProcess h1
  putStrLn ""
  putStrLn "GHC version: "
  h2 <- runProcess "../dist/build/haddock/haddock" ["--ghc-version"] Nothing (Just [("haddock_datadir", "../.")]) Nothing Nothing Nothing
  waitForProcess h2
  putStrLn ""

  libdir <- rawSystemStdout normal "../dist/build/haddock/haddock" ["--print-ghc-libdir"]
  let basepath = init libdir ++ "/../../share/doc/ghc/html/libraries/base/"
  let base = "-i " ++ basepath ++ "," ++ basepath ++ "base.haddock"
  let processpath = init libdir ++ "/../../share/doc/ghc/html/libraries/process/"
  let process = "-i " ++ processpath ++ "," ++ processpath ++ "process.haddock"

  putStrLn "Running tests..."
  handle <- runProcess "../dist/build/haddock/haddock" (["-w", "-o", outdir, "-h", "--optghc=-fglasgow-exts", "--optghc=-w", base, process] ++ mods') Nothing (Just [("haddock_datadir", "../.")]) Nothing Nothing Nothing
  code <- waitForProcess handle
  when (code /= ExitSuccess) $ error "Haddock run failed! Exiting."
  check mods (if not (null args) && args !! 0 == "all" then False else True)

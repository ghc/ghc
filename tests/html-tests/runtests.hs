import Control.Monad
import Data.List
import Data.Maybe
import Distribution.Simple.Program
import Distribution.Simple.Utils
import Distribution.Verbosity
import System.Cmd
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.Process
import Text.Printf
import Text.Regex


packageRoot   = "."
haddockPath   = packageRoot </> "dist" </> "build" </> "haddock" </> "haddock"
testSuiteRoot = packageRoot </> "tests" </> "html-tests"
testDir       = testSuiteRoot </> "tests"
outDir        = testSuiteRoot </> "output"


main = do
  test
  putStrLn "All tests passed!"


test = do
  x <- doesFileExist haddockPath
  when (not x) $ die "you need to run 'cabal build' successfully first"

  contents <- getDirectoryContents testDir
  args <- getArgs
  let (opts, spec) = span ("-" `isPrefixOf`) args
  let mods =
        case spec of
          x:_ | x /= "all" -> [x ++ ".hs"]
          _ -> filter ((==) ".hs" . takeExtension) contents

  let mods' = map (testDir </>) mods
  putStrLn ""
  putStrLn "Haddock version: "
  h1 <- runProcess haddockPath ["--version"] Nothing
                   (Just [("haddock_datadir", packageRoot)]) Nothing Nothing Nothing
  waitForProcess h1
  putStrLn ""
  putStrLn "GHC version: "
  h2 <- runProcess haddockPath ["--ghc-version"] Nothing
                   (Just [("haddock_datadir", packageRoot)]) Nothing Nothing Nothing
  waitForProcess h2
  putStrLn ""

  -- TODO: use Distribution.* to get the packages instead
  libdir <- rawSystemStdout normal haddockPath ["--print-ghc-libdir"]
  let librariesPath = ".."</>".."</>"share"</>"doc"</>"ghc"</>"html"</>"libraries"

  let mkDep name version =
        let path = init libdir </> librariesPath </> name ++ "-" ++ version
        in  "-i " ++ path ++ "," ++ path </> name ++ ".haddock"

  let base    = mkDep "base" "4.3.1.0"
      process = mkDep "process" "1.0.1.5"
      ghcprim = mkDep "ghc-prim" "0.2.0.0"

  putStrLn "Running tests..."
  handle <- runProcess haddockPath
                       (["-w", "-o", outDir, "-h", "--pretty-html", "--optghc=-fglasgow-exts"
                        , "--optghc=-w", base, process, ghcprim] ++ opts ++ mods')
                       Nothing (Just [("haddock_datadir", packageRoot)]) Nothing
                       Nothing Nothing

  code <- waitForProcess handle
  when (code /= ExitSuccess) $ error "Haddock run failed! Exiting."
  check mods (if not (null args) && args !! 0 == "all" then False else True)


check modules strict = do
  forM_ modules $ \mod -> do
    let outfile = outDir  </> dropExtension mod ++ ".html"
    let reffile = testDir </> dropExtension mod ++ ".html.ref"
    b <- doesFileExist reffile
    if b
      then do
        copyFile reffile (outDir </> takeFileName reffile)
        out <- readFile outfile
        ref <- readFile reffile
        if not $ haddockEq out ref
          then do
            putStrLn $ "Output for " ++ mod ++ " has changed! Exiting with diff:"
            let ref' = stripLinks ref
                out' = stripLinks out
            let reffile' = outDir </> takeFileName reffile ++ ".nolinks"
                outfile' = outDir </> takeFileName outfile ++ ".nolinks"
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


haddockEq file1 file2 = stripLinks file1 == stripLinks file2


stripLinks f = subRegex (mkRegexWithOpts "<A HREF=[^>]*>" False False) f "<A HREF=\"\">"


programOnPath p = do
  result <- findProgramLocation silent p
  return (isJust result)


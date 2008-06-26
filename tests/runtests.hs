import System.Cmd
import System.Environment
import System.FilePath
import System.Exit
import System.Directory
import Data.List
import Control.Monad
import Text.Printf
import Text.Regex


main = do
  putStrLn "Running tests..."
  test
  putStrLn "All tests passed!"


haddockEq file1 file2 = (stripLinks file1) == (stripLinks file2)
  where
    stripLinks f = subRegex (mkRegexWithOpts "<A HREF=[^>]*>" False False) f "<A HREF=\"\">"

check modules = do
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
            system $ "diff " ++ reffile ++ " " ++ outfile
            exitFailure
          else do
            putStrLn $ "Pass: " ++ mod
      else do
        putStrLn $ "Pass: " ++ mod ++ " (no .ref file)"
 

test = do
  contents <- getDirectoryContents "tests"
  let mods = filter ((==) ".hs" . takeExtension) contents
  let outdir = "output"
  let mods' = map ("tests" </>) mods
  code <- system $ printf "haddock -w -o %s -h --optghc=-fglasgow-exts --optghc=-w %s" outdir (unwords mods')
  unless (code == ExitSuccess) $ error "Haddock run failed! Exiting."
  check mods

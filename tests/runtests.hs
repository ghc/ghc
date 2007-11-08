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
  args <- getArgs
  when (null args) $ error "You must give the path to the GHC lib dir as an argument"  
  putStrLn "Running tests..."
  let libdir = head args
  walkDirs libdir "."
  putStrLn "All tests passed!"


haddockEq file1 file2 = (stripLinks file1) == (stripLinks file2)
  where
    stripLinks f = subRegex (mkRegexWithOpts "<A HREF=[^>]*>" False False) f "<A HREF=\"\">"


allModules dir = do
  contents <- getDirectoryContents dir
  return $ filter ((==) ".hs" . takeExtension) contents


check modules = do
  forM_ modules $ \mod -> do
    let outfile = "output" </> (dropExtension mod ++ ".html")
    let reffile = dropExtension mod ++ ".html.ref"
    b <- doesFileExist reffile
    if b 
      then do
        copyFile reffile ("output" </> reffile)
        out <- readFile outfile
        ref <- readFile reffile
        if not $ haddockEq out ref
          then do
            putStrLn $ "Output for " ++ mod ++ " has changed! Exiting."
            exitFailure
          else do
            putStrLn $ "Pass: " ++ mod
      else do
        putStrLn $ "Pass: " ++ mod ++ " (no .ref file)"
 

walkDirs libdir basedir = do
  contents <- getDirectoryContents basedir
  dirs <- filterM doesDirectoryExist . 
          map (basedir </>) .
          filter (`notElem` [".", "..", "output"]) $ contents
  mapM_ (testDir libdir) dirs


testDir libdir dir = do
  mods <- allModules dir
  let mods' = map (dir </>) mods
  let outdir = "output" </> dir
  createDirectoryIfMissing True outdir
  code <- system $ printf "haddock -B %s -o %s -h --optghc=-fglasgow-exts %s" libdir outdir (unwords mods')
  unless (code == ExitSuccess) $ error "Haddock run failed! Exiting."
  check mods'
  walkDirs libdir dir

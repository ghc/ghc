
module Main (main) where

import Control.Monad
import Data.List
import System.Environment
import System.Exit
import System.FilePath
import System.IO

data CleanWhat = CleanFile FilePath
               | CleanRec  FilePath
    deriving (Read, Show)

main :: IO ()
main = do args <- getArgs
          case args of
              [contentsBeforeFile, contentsAfterFile, wouldBeCleanedFile] ->
                  doit contentsBeforeFile contentsAfterFile wouldBeCleanedFile
              _ ->
                  error "Bad args"

doit :: FilePath -> FilePath -> FilePath -> IO ()
doit contentsBeforeFile contentsAfterFile wouldBeCleanedFile
 = do contentsBefore <- liftM lines $ readFile contentsBeforeFile
      contentsAfter  <- liftM lines $ readFile contentsAfterFile
      wouldBeCleaned <- liftM (map read . lines) $ readFile wouldBeCleanedFile
      let newContentsAfter = contentsAfter \\ contentsBefore
      let cleanedAfter = simulateCleans newContentsAfter wouldBeCleaned
      unless (null cleanedAfter) $ do
          hPutStrLn stderr "Files not cleaned:"
          mapM_ (hPutStrLn stderr . show) cleanedAfter
          exitWith (ExitFailure 1)

simulateCleans :: [FilePath] -> [CleanWhat] -> [FilePath]
simulateCleans fs cws = filter (not . cleaned) fs
    where cleaned f = any (`willClean` f) cws

willClean :: CleanWhat -> FilePath -> Bool
CleanFile fp `willClean` f = fp `equalFilePath` f
CleanRec fp `willClean` f
    = any (fp `equalFilePath`) (map joinPath $ inits $ splitPath f)


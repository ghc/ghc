
module Main (main) where

import Control.Monad
import Data.List
import qualified Data.Set as Set
import Data.Set (Set)
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

readSet :: FilePath -> IO (Set FilePath)
readSet fp = liftM (Set.fromList . lines) $ readFile fp

doit :: FilePath -> FilePath -> FilePath -> IO ()
doit contentsBeforeFile contentsAfterFile wouldBeCleanedFile
 = do contentsBefore <- readSet contentsBeforeFile
      contentsAfter  <- readSet contentsAfterFile
      wouldBeCleaned <- liftM (map read . lines) $ readFile wouldBeCleanedFile
      let newContentsAfter = contentsAfter `Set.difference` contentsBefore
      let cleanedAfter = simulateCleans newContentsAfter wouldBeCleaned
      unless (Set.null cleanedAfter) $ do
          hPutStrLn stderr "Files not cleaned:"
          mapM_ (hPutStrLn stderr . show) (Set.toList cleanedAfter)
          exitWith (ExitFailure 1)

simulateCleans :: Set FilePath -> [CleanWhat] -> Set FilePath
simulateCleans fs cws = Set.filter (not . cleaned) fs
    where cleaned f = any (`willClean` f) cws

willClean :: CleanWhat -> FilePath -> Bool
CleanFile fp `willClean` f = fp `equalFilePath` f
CleanRec fp `willClean` f
    = any (fp `equalFilePath`) (map joinPath $ inits $ splitPath f)


{-
   This is a simple test program for file metadata. It tests a few
   operations on files and directories, to ensure that our compiler
   and filesystem produce sensible results.

   If this test fails, it is likely that cabal or backpack tests
   will fail too.

   Some properties tested:

      * temporary files are regular files and not directories
      * the current directory is a directory
      * file size of small temporary files is correct
      * modification time of created temporary files is close to current time (60s)
      * modification time of a second temporary file is later than the first

 -}
{-# LANGUAGE CPP #-}

import Control.Monad (replicateM_)

import Data.Time.Clock
import System.IO
import qualified System.Directory as D
import System.IO.Error
import Control.Exception
import Control.Concurrent (threadDelay)

#if !defined(mingw32_HOST_OS)
import qualified System.Posix.Files as P
import Data.Time.Clock.POSIX
#endif

data FileInfo = FileInfo { fiSize          :: Integer
                         , fiModified      :: UTCTime
                         , fiIsRegularFile :: Bool
                         , fiIsDirectory   :: Bool
                         } deriving (Eq, Show)

testFile1, testFile2 :: FilePath
testFile1 = "test1.out"
testFile2 = "test2.out"

main :: IO ()
main = do
  putStrLn ("checking file " ++ testFile1)
  handleFileSize1 <- withBinaryFile testFile1 WriteMode $ \h -> do
    replicateM_ 50 (hPutChar h 'a')
    hFileSize h
  fi1 <- getFileInfo testFile1
  D.removeFile testFile1
  putStrLn ("handle file size: " ++ show handleFileSize1)
  currentTime1 <- getCurrentTime
  printFileInfo currentTime1 fi1

  putStrLn ("\nchecking current directory")
  currentDir <- D.getCurrentDirectory
  di  <- getFileInfo currentDir
  putStrLn ("is regular file: " ++ show (fiIsRegularFile di))
  putStrLn ("is directory: " ++ show (fiIsDirectory di))

  -- wait two seconds before testing the second file
  threadDelay 2000000

  putStrLn ("\nchecking file " ++ testFile2)
  handleFileSize2 <- withBinaryFile testFile2 WriteMode $ \h -> do
    replicateM_ 75 (hPutChar h 'b')
    hFileSize h
  fi2 <- getFileInfo testFile2
  D.removeFile testFile2
  currentTime2 <- getCurrentTime
  putStrLn ("handle file size: " ++ show handleFileSize2)
  printFileInfo currentTime2 fi2

  -- check that the second file was modified after the first
  putStrLn ("second file modified after first: " ++ show (diffUTCTime (fiModified fi2) (fiModified fi1) >= 1))


printFileInfo :: UTCTime -> FileInfo -> IO ()
printFileInfo time fi = do
  putStrLn $ "file size: " ++ show (fiSize fi)
  putStrLn $ "is regular file: " ++ show (fiIsRegularFile fi)
  putStrLn $ "is directory: " ++ show (fiIsDirectory fi)
  putStrLn $ "time stamp close enough: " ++ show (closeEnough time (fiModified fi))

getFileInfo :: FilePath -> IO FileInfo
getFileInfo path = do
  -- get some basic info about the path
  dirExists  <- D.doesDirectoryExist path
  fileExists <- D.doesFileExist path
  fileSize   <- if fileExists then D.getFileSize path else pure 0
  modTime    <- D.getModificationTime path
#if !defined(mingw32_HOST_OS)
  -- check against unix package (which uses a different way to access some fields of the stat structure)
  fs <- P.getFileStatus path
  check "isRegularFile" (P.isRegularFile fs == fileExists)
  check "isDirectory"   (P.isDirectory fs   == dirExists)
  check "modificationTime" (closeEnough (posixSecondsToUTCTime (realToFrac (P.modificationTime fs))) modTime)
  check "fileSize"      (fromIntegral (P.fileSize fs) == fileSize || not fileExists)
#endif
  pure (FileInfo fileSize modTime fileExists dirExists)


check :: String -> Bool -> IO ()
check err False = throwIO (userError err)
check _   True  = pure ()

closeEnough :: UTCTime -> UTCTime -> Bool
closeEnough a b = abs (diffUTCTime a b) < 60


{-# LANGUAGE CPP #-}
module Utils
  ( prepBenchmarkFiles -- :: IO ()
  ) where

import           Data.Time
import           System.IO (hFlush, stdout)
import           System.Mem
import           Control.Monad (when)
import           System.FilePath
import           System.Directory

import qualified Data.ByteString        as B
import qualified Data.ByteString.Lazy   as BS
import qualified Codec.Compression.GZip as GZip

import           Macro.DeepSeq ()
import qualified Macro.Load      as Load
import qualified Macro.PkgBinary as PkgBinary
import qualified Macro.PkgCereal as PkgCereal
import qualified Macro.PkgStore  as PkgStore
import qualified Macro.CBOR      as CBOR

--------------------------------------------------------------------------------

-- | Get the path to the Hackage.haskell.org package index. Works on Windows
-- and Linux.
getHackageIndexLocation :: IO FilePath
getHackageIndexLocation = do
  cabalDir <- getAppUserDataDirectory "cabal"
  let dir = cabalDir </> "packages" </> "hackage.haskell.org"
  return (dir </> "01-index.tar.gz")

-- | Copy the hackage index to a local directory. Returns the path to the
-- directory containing the file, and the file path itself.
copyHackageIndex :: IO (FilePath, FilePath)
copyHackageIndex = do
  hackageIndex <- getHackageIndexLocation

  let dataDir = "bench" </> "data"
      dest    = dataDir </> "01-index.tar.gz"

  -- Create the data dir, and copy the index.
  -- We do not try to create the 'bench' directory since it should exist.
  createDirectoryIfMissing False dataDir
  exists <- doesFileExist dest
  when (not exists) $ do
    notice "Copying hackage index" (copyFile hackageIndex dest)

  return (dataDir, dest)

-- | Prepare all the files needed for the benchmarks to properly run,
-- including needing a copy of the Hackage index, and a few encodings
-- of some of its contents
prepBenchmarkFiles :: IO ()
prepBenchmarkFiles = do
  -- Set up index
  (destDir, indexFile) <- copyHackageIndex

  -- Read it, and take about 20,000 entries. And a small set of 1,000 too.
  let readIndex = Load.readPkgIndex . GZip.decompress
  Right pkgs_ <- fmap readIndex (BS.readFile indexFile)
  let _pkgs1k  = take 1000  pkgs_
      _pkgs20k = take 20000 pkgs_

      -- Write a file to the temporary directory, if it does not exist.
      write p bs = do
        let file = destDir </> p
        exists <- doesFileExist file
        when (not exists) $ do
          let msg = "Creating " ++ file
          notice msg (BS.writeFile file bs)
      -- Write a file to the temporary directory, if it does not exist.
      -- Strict version.
      writeS p bs = do
        let file = destDir </> p
        exists <- doesFileExist file
        when (not exists) $ do
          let msg = "Creating " ++ file
          notice msg (B.writeFile file bs)

  -- Encode that dense data in several forms, and write those forms out
  -- to disk. TODO FIXME (aseipp): should we actually have the -small variant?
  -- It doesn't have any similar benchmarks for the other libraries, and seems
  -- like it isn't very useful, given the other ones.
  write "binary.bin"     (PkgBinary.serialise _pkgs20k)
  write "cereal.bin"     (PkgCereal.serialise _pkgs20k)
  write "cbor.bin"       (CBOR.serialise      _pkgs20k)
  writeS "store.bin"     (PkgStore.serialise  _pkgs20k)
--write "cbor-small.bin" (CBOR.serialise      _pkgs1k)

  -- And before we finish: do a garbage collection to clean up anything left
  -- over.
  notice "Preparation done; performing GC" doGC

--------------------------------------------------------------------------------

-- | Do a garbage collection.
doGC :: IO ()
#if MIN_VERSION_base(4,7,0)
doGC = performMinorGC >> performMajorGC
#else
doGC = performGC
#endif

-- Write a notice to the screen (with timing information).
notice :: String -> IO a -> IO a
notice m k = do
  putStr ("INFO: " ++ m ++ "... ")
  hFlush stdout
  (v,t) <- timeIt k
  putStrLn $ "OK (in " ++ show t ++ ")"
  return v

-- | Time some action and return the time difference.
timeIt :: IO a -> IO (a, NominalDiffTime)
timeIt action = do
  t   <- getCurrentTime
  x   <- action
  t'  <- getCurrentTime
  return (x, diffUTCTime t' t)

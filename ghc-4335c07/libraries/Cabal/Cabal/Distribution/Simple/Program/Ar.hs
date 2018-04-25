{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NondecreasingIndentation #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.Program.Ar
-- Copyright   :  Duncan Coutts 2009
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- This module provides an library interface to the @ar@ program.

module Distribution.Simple.Program.Ar (
    createArLibArchive,
    multiStageProgramInvocation
  ) where

import Prelude ()
import Distribution.Compat.Prelude

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Distribution.Compat.CopyFile (filesEqual)
import Distribution.Simple.Compiler (arResponseFilesSupported)
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo(..))
import Distribution.Simple.Program
         ( ProgramInvocation, arProgram, requireProgram )
import Distribution.Simple.Program.ResponseFile
         ( withResponseFile )
import Distribution.Simple.Program.Run
         ( programInvocation, multiStageProgramInvocation
         , runProgramInvocation )
import Distribution.Simple.Setup
         ( fromFlagOrDefault, configUseResponseFiles )
import Distribution.Simple.Utils
         ( defaultTempFileOptions, dieWithLocation', withTempDirectory )
import Distribution.System
         ( Arch(..), OS(..), Platform(..) )
import Distribution.Verbosity
         ( Verbosity, deafening, verbose )
import System.Directory (doesFileExist, renameFile)
import System.FilePath ((</>), splitFileName)
import System.IO
         ( Handle, IOMode(ReadWriteMode), SeekMode(AbsoluteSeek)
         , hFileSize, hSeek, withBinaryFile )

-- | Call @ar@ to create a library archive from a bunch of object files.
--
createArLibArchive :: Verbosity -> LocalBuildInfo
                   -> FilePath -> [FilePath] -> IO ()
createArLibArchive verbosity lbi targetPath files = do
  (ar, _) <- requireProgram verbosity arProgram progDb

  let (targetDir, targetName) = splitFileName targetPath
  withTempDirectory verbosity targetDir "objs" $ \ tmpDir -> do
  let tmpPath = tmpDir </> targetName

  -- The args to use with "ar" are actually rather subtle and system-dependent.
  -- In particular we have the following issues:
  --
  --  -- On OS X, "ar q" does not make an archive index. Archives with no
  --     index cannot be used.
  --
  --  -- GNU "ar r" will not let us add duplicate objects, only "ar q" lets us
  --     do that. We have duplicates because of modules like "A.M" and "B.M"
  --     both make an object file "M.o" and ar does not consider the directory.
  --
  -- Our solution is to use "ar r" in the simple case when one call is enough.
  -- When we need to call ar multiple times we use "ar q" and for the last
  -- call on OSX we use "ar qs" so that it'll make the index.

  let simpleArgs  = case hostOS of
             OSX -> ["-r", "-s"]
             _   -> ["-r"]

      initialArgs = ["-q"]
      finalArgs   = case hostOS of
             OSX -> ["-q", "-s"]
             _   -> ["-q"]

      extraArgs   = verbosityOpts verbosity ++ [tmpPath]

      simple  = programInvocation ar (simpleArgs  ++ extraArgs)
      initial = programInvocation ar (initialArgs ++ extraArgs)
      middle  = initial
      final   = programInvocation ar (finalArgs   ++ extraArgs)

      oldVersionManualOverride =
        fromFlagOrDefault False $ configUseResponseFiles $ configFlags lbi
      responseArgumentsNotSupported   =
        not (arResponseFilesSupported (compiler lbi))

      invokeWithResponesFile :: FilePath -> ProgramInvocation
      invokeWithResponesFile atFile =
        programInvocation ar $
        simpleArgs ++ extraArgs ++ ['@' : atFile]

  if oldVersionManualOverride || responseArgumentsNotSupported
    then
      sequence_
        [ runProgramInvocation verbosity inv
        | inv <- multiStageProgramInvocation
                   simple (initial, middle, final) files ]
    else
      withResponseFile verbosity defaultTempFileOptions tmpDir "ar.rsp" Nothing files $
        \path -> runProgramInvocation verbosity $ invokeWithResponesFile path

  unless (hostArch == Arm -- See #1537
          || hostOS == AIX) $ -- AIX uses its own "ar" format variant
    wipeMetadata verbosity tmpPath
  equal <- filesEqual tmpPath targetPath
  unless equal $ renameFile tmpPath targetPath

  where
    progDb = withPrograms lbi
    Platform hostArch hostOS = hostPlatform lbi
    verbosityOpts v
      | v >= deafening = ["-v"]
      | v >= verbose   = []
      | otherwise      = ["-c"] -- Do not warn if library had to be created.

-- | @ar@ by default includes various metadata for each object file in their
-- respective headers, so the output can differ for the same inputs, making
-- it difficult to avoid re-linking. GNU @ar@(1) has a deterministic mode
-- (@-D@) flag that always writes zero for the mtime, UID and GID, and 0644
-- for the file mode. However detecting whether @-D@ is supported seems
-- rather harder than just re-implementing this feature.
wipeMetadata :: Verbosity -> FilePath -> IO ()
wipeMetadata verbosity path = do
    -- Check for existence first (ReadWriteMode would create one otherwise)
    exists <- doesFileExist path
    unless exists $ wipeError "Temporary file disappeared"
    withBinaryFile path ReadWriteMode $ \ h -> hFileSize h >>= wipeArchive h

  where
    wipeError msg = dieWithLocation' verbosity path Nothing $
        "Distribution.Simple.Program.Ar.wipeMetadata: " ++ msg
    archLF = "!<arch>\x0a" -- global magic, 8 bytes
    x60LF = "\x60\x0a" -- header magic, 2 bytes
    metadata = BS.concat
        [ "0           " -- mtime, 12 bytes
        , "0     " -- UID, 6 bytes
        , "0     " -- GID, 6 bytes
        , "0644    " -- mode, 8 bytes
        ]
    headerSize :: Int
    headerSize = 60

    -- http://en.wikipedia.org/wiki/Ar_(Unix)#File_format_details
    wipeArchive :: Handle -> Integer -> IO ()
    wipeArchive h archiveSize = do
        global <- BS.hGet h (BS.length archLF)
        unless (global == archLF) $ wipeError "Bad global header"
        wipeHeader (toInteger $ BS.length archLF)

      where
        wipeHeader :: Integer -> IO ()
        wipeHeader offset = case compare offset archiveSize of
            EQ -> return ()
            GT -> wipeError (atOffset "Archive truncated")
            LT -> do
                header <- BS.hGet h headerSize
                unless (BS.length header == headerSize) $
                    wipeError (atOffset "Short header")
                let magic = BS.drop 58 header
                unless (magic == x60LF) . wipeError . atOffset $
                    "Bad magic " ++ show magic ++ " in header"

                let name = BS.take 16 header
                let size = BS.take 10 $ BS.drop 48 header
                objSize <- case reads (BS8.unpack size) of
                    [(n, s)] | all isSpace s -> return n
                    _ -> wipeError (atOffset "Bad file size in header")

                let replacement = BS.concat [ name, metadata, size, magic ]
                unless (BS.length replacement == headerSize) $
                    wipeError (atOffset "Something has gone terribly wrong")
                hSeek h AbsoluteSeek offset
                BS.hPut h replacement

                let nextHeader = offset + toInteger headerSize +
                        -- Odd objects are padded with an extra '\x0a'
                        if odd objSize then objSize + 1 else objSize
                hSeek h AbsoluteSeek nextHeader
                wipeHeader nextHeader

          where
            atOffset msg = msg ++ " at offset " ++ show offset

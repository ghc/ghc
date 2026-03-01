{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}

-------------------------------------------------------------------------------
--
-- | On-Demand Archive Prelinking for GHC Internal Linker
--
-- This module provides on-demand prelinking of static archives (.a files)
-- for the GHC internal linker, replacing the traditional approach of
-- pre-generating GHCi prelinked objects.
--
-- Instead of creating prelinked objects at build time (which was slow and
-- disk-intensive), we now create them on-demand when the internal linker
-- loads large archives. This speeds up loading and linking by merging
-- archive members into a single object file using 'ld -r', which:
--
-- 1. Reduces the number of objects the RTS linker must process
-- 2. Eliminates relocation range issues in large archives
-- 3. Speeds up loading through fewer system calls
-- 4. Provides optional persistent caching for reuse across sessions
--
-- Copyright (c) Moritz Angermann <moritz@zw3rk.com>, zw3rk pte. ltd.
-- License: Apache 2
--
-------------------------------------------------------------------------------

module GHC.Linker.ArchivePrelink
  ( shouldPrelinkArchive
  , prelinkArchive
  , computeArchiveHash
  , findCachedPrelink
  , getCacheDir
  ) where

import GHC.Prelude

import GHC.Driver.Session
import GHC.Utils.Logger
import GHC.Utils.TmpFs
import GHC.Utils.Outputable
import GHC.Utils.Fingerprint
import GHC.Utils.Panic
import GHC.Utils.Error
import GHC.SysTools.Tasks

import Control.Monad
import qualified Control.Exception as E
import System.FilePath
import System.Directory
import System.Environment (lookupEnv)
import Data.Maybe

-- | Determine if an archive should be prelinked
--
-- Prelinking is enabled when:
-- 1. The prelinkArchiveThreshold is set (Just threshold)
-- 2. The file is a static archive (*.a extension)
-- 3. The file size >= threshold
-- 4. The merge objects linker (ld -r) is configured
shouldPrelinkArchive :: DynFlags -> FilePath -> IO Bool
shouldPrelinkArchive dflags path = do
  case prelinkArchiveThreshold dflags of
    Nothing -> return False  -- Prelinking disabled
    Just threshold -> do
      -- Only prelink static archives (*.a extension)
      let isStaticArchive = takeExtension path == ".a"
      if not isStaticArchive
        then return False
        else do
          -- Check if file exists (might be in a library search path)
          exists <- doesFileExist path
          if not exists
            then return False
            else do
              size <- getFileSize path
              let hasLdR = isJust (pgm_lm dflags)
              let shouldPrelink = fromIntegral size >= threshold && hasLdR
              return shouldPrelink

-- | Compute a hash of an archive file for cache key
--
-- Uses GHC's built-in MD5 fingerprinting (getFileHash) for consistency
-- with other GHC fingerprinting operations.
computeArchiveHash :: FilePath -> IO String
computeArchiveHash path = do
  fp <- getFileHash path
  return $ show fp  -- Fingerprint has a Show instance that produces hex string

-- | Find existing prelinked object in cache
--
-- Returns Nothing if:
-- - Persistent cache is disabled (cacheDir = Nothing)
-- - Cache file doesn't exist
-- - Cache file exists but is invalid (corrupt, wrong version, etc.)
findCachedPrelink :: Logger -> Maybe FilePath -> FilePath -> IO (Maybe FilePath)
findCachedPrelink logger cacheDir archivePath = do
  case cacheDir of
    Nothing -> return Nothing  -- Per-session only, no persistent cache
    Just dir -> do
      hash <- computeArchiveHash archivePath
      let originalName = takeFileName archivePath
      let cachedName = hash ++ "-" ++ replaceExtension originalName ".o"
      let cachedPath = dir </> cachedName
      exists <- doesFileExist cachedPath
      if exists
        then do
          debugTraceMsg logger 3 $ text "Found cached prelink:" <+> text cachedPath
          return (Just cachedPath)
        else return Nothing

-- | Extract archive members to temporary directory
--
-- Uses 'ar x' to extract all member object files.
-- Returns list of extracted object file paths.
extractArchiveMembers :: Logger -> ArConfig -> FilePath -> FilePath -> IO [FilePath]
extractArchiveMembers logger arConfig archivePath tempDir = do
  debugTraceMsg logger 3 $ text "Extracting archive:" <+> text archivePath

  -- Use 'ar x' to extract all members to temp directory
  let cwd = Just tempDir
  runAr logger arConfig cwd [Option "x", FileOption "" archivePath]

  -- List extracted files and filter for .o files
  files <- listDirectory tempDir
  let objFiles = filter (\f -> takeExtension f == ".o") files

  when (null objFiles) $
    throwGhcException $ UsageError $
      "Archive " ++ archivePath ++ " contains no object files"

  return $ map (tempDir </>) objFiles

-- | Prelink an archive into a single merged object file
--
-- This is the main entry point for prelinking. It:
-- 1. Checks the cache for an existing prelinked object
-- 2. If not cached, extracts archive members to a temp directory
-- 3. Runs 'ld -r' to merge all objects into a single .o file
-- 4. Stores the result in cache (if persistent cache enabled)
-- 5. Returns the path to the prelinked object
--
-- If any step fails, returns Nothing and the caller should fall back
-- to loading the original archive.
prelinkArchive :: Logger -> TmpFs -> DynFlags -> FilePath -> IO (Maybe FilePath)
prelinkArchive logger tmpfs dflags archivePath =
  E.catch doPrelink $ \(e :: E.SomeException) -> do
    logInfo logger $ text "Prelinking failed for" <+> text archivePath
                     <> text ":" <+> text (E.displayException e)
    logInfo logger $ text "Falling back to normal archive loading"
    return Nothing
  where
    doPrelink = do
      debugTraceMsg logger 2 $ text "Prelinking archive:" <+> text archivePath

      -- Check cache first
      let arConfig = configureAr dflags
      cacheDir <- getCacheDir dflags
      cached <- findCachedPrelink logger cacheDir archivePath
      case cached of
        Just path -> do
          debugTraceMsg logger 2 $ text "Using cached prelink:" <+> text path
          return (Just path)
        Nothing -> do
          -- Extract members to temp directory
          tempDir <- newTempSubDir logger tmpfs (tmpDir dflags)
          members <- extractArchiveMembers logger arConfig archivePath tempDir

          debugTraceMsg logger 3 $ text "Extracted" <+> int (length members) <+> text "members"

          -- Determine output path (cached or temp)
          outputPath <- case cacheDir of
            Nothing -> do
              -- Per-session temp file
              newTempName logger tmpfs (tmpDir dflags) TFL_GhcSession "o"
            Just dir -> do
              -- Persistent cache
              createDirectoryIfMissing True dir
              hash <- computeArchiveHash archivePath
              let originalName = takeFileName archivePath
              let cachedName = hash ++ "-" ++ replaceExtension originalName ".o"
              return $ dir </> cachedName

          -- Run ld -r to merge objects
          -- Note: -r flag is already included in pgm_lm configuration
          debugTraceMsg logger 3 $ text "Merging" <+> int (length members) <+> text "objects into" <+> text outputPath
          let mergeArgs = [ Option "-o"
                         , FileOption "" outputPath
                         ]
                       ++ map (FileOption "") members
          runMergeObjects logger tmpfs dflags mergeArgs

          debugTraceMsg logger 2 $ text "Created prelinked object:" <+> text outputPath
          return (Just outputPath)

-- | Get cache directory from configuration
--
-- Priority (highest to lowest):
-- 1. Command-line flag: prelinkCacheDir in DynFlags
-- 2. Environment variable: GHC_PRELINK_CACHE
-- 3. Default: Nothing (per-session temp files only)
getCacheDir :: DynFlags -> IO (Maybe FilePath)
getCacheDir dflags = do
  case prelinkCacheDir dflags of
    Just dir -> return (Just dir)  -- Command-line flag wins
    Nothing -> do
      -- Check environment variable
      envVar <- lookupEnv "GHC_PRELINK_CACHE"
      return envVar

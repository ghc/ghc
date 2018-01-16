{-# LANGUAGE CPP #-}
{-# LANGUAGE NondecreasingIndentation #-}
{-# LANGUAGE FlexibleContexts #-}
-- Implements the \"@.\/cabal sdist@\" command, which creates a source
-- distribution for this package.  That is, packs up the source code
-- into a tarball, making use of the corresponding Cabal module.
module Distribution.Client.SrcDist (
         sdist,
         allPackageSourceFiles
  )  where


import Distribution.Client.SetupWrapper
        ( SetupScriptOptions(..), defaultSetupScriptOptions, setupWrapper )
import Distribution.Client.Tar (createTarGzFile)

import Distribution.Package
         ( Package(..), packageName )
import Distribution.PackageDescription
         ( PackageDescription )
import Distribution.PackageDescription.Configuration
         ( flattenPackageDescription )
import Distribution.PackageDescription.Parsec
         ( readGenericPackageDescription )
import Distribution.Simple.Utils
         ( createDirectoryIfMissingVerbose, defaultPackageDesc
         , warn, die', notice, withTempDirectory )
import Distribution.Client.Setup
         ( SDistFlags(..), SDistExFlags(..), ArchiveFormat(..) )
import Distribution.Simple.Setup
         ( Flag(..), sdistCommand, flagToList, fromFlag, fromFlagOrDefault
         , defaultSDistFlags )
import Distribution.Simple.BuildPaths ( srcPref)
import Distribution.Simple.Program (requireProgram, simpleProgram, programPath)
import Distribution.Simple.Program.Db (emptyProgramDb)
import Distribution.Text ( display )
import Distribution.Verbosity (Verbosity, normal, lessVerbose)
import Distribution.Version   (mkVersion, orLaterVersion, intersectVersionRanges)

import Distribution.Client.Utils
  (tryFindAddSourcePackageDesc)
import Distribution.Compat.Exception                 (catchIO)

import System.FilePath ((</>), (<.>))
import Control.Monad (when, unless, liftM)
import System.Directory (doesFileExist, removeFile, canonicalizePath, getTemporaryDirectory)
import System.Process (runProcess, waitForProcess)
import System.Exit    (ExitCode(..))
import Control.Exception                             (IOException, evaluate)

-- |Create a source distribution.
sdist :: SDistFlags -> SDistExFlags -> IO ()
sdist flags exflags = do
  pkg <- liftM flattenPackageDescription
    (readGenericPackageDescription verbosity =<< defaultPackageDesc verbosity)
  let withDir :: (FilePath -> IO a) -> IO a
      withDir = if not needMakeArchive then \f -> f tmpTargetDir
                else withTempDirectory verbosity tmpTargetDir "sdist."
  -- 'withTempDir' fails if we don't create 'tmpTargetDir'...
  when needMakeArchive $
    createDirectoryIfMissingVerbose verbosity True tmpTargetDir
  withDir $ \tmpDir -> do
    let outDir = if isOutDirectory then tmpDir else tmpDir </> tarBallName pkg
        flags' = (if not needMakeArchive then flags
                  else flags { sDistDirectory = Flag outDir })
    unless isListSources $
      createDirectoryIfMissingVerbose verbosity True outDir

    -- Run 'setup sdist --output-directory=tmpDir' (or
    -- '--list-source'/'--output-directory=someOtherDir') in case we were passed
    -- those options.
    setupWrapper verbosity setupOpts (Just pkg) sdistCommand (const flags') []

    -- Unless we were given --list-sources or --output-directory ourselves,
    -- create an archive.
    when needMakeArchive $
      createArchive verbosity pkg tmpDir distPref

    when isOutDirectory $
      notice verbosity $ "Source directory created: " ++ tmpTargetDir

    when isListSources $
      notice verbosity $ "List of package sources written to file '"
                         ++ (fromFlag . sDistListSources $ flags) ++ "'"

  where
    flagEnabled f  = not . null . flagToList . f $ flags

    isListSources   = flagEnabled sDistListSources
    isOutDirectory  = flagEnabled sDistDirectory
    needMakeArchive = not (isListSources || isOutDirectory)
    verbosity       = fromFlag (sDistVerbosity flags)
    distPref        = fromFlag (sDistDistPref flags)
    tmpTargetDir    = fromFlagOrDefault (srcPref distPref) (sDistDirectory flags)
    setupOpts       = defaultSetupScriptOptions {
      useDistPref     = distPref,
      -- The '--output-directory' sdist flag was introduced in Cabal 1.12, and
      -- '--list-sources' in 1.17.
      useCabalVersion = if isListSources
                        then orLaterVersion $ mkVersion [1,17,0]
                        else orLaterVersion $ mkVersion [1,12,0]
      }
    format        = fromFlag (sDistFormat exflags)
    createArchive = case format of
      TargzFormat -> createTarGzArchive
      ZipFormat   -> createZipArchive

tarBallName :: PackageDescription -> String
tarBallName = display . packageId

-- | Create a tar.gz archive from a tree of source files.
createTarGzArchive :: Verbosity -> PackageDescription -> FilePath -> FilePath
                    -> IO ()
createTarGzArchive verbosity pkg tmpDir targetPref = do
    createTarGzFile tarBallFilePath tmpDir (tarBallName pkg)
    notice verbosity $ "Source tarball created: " ++ tarBallFilePath
  where
    tarBallFilePath = targetPref </> tarBallName pkg <.> "tar.gz"

-- | Create a zip archive from a tree of source files.
createZipArchive :: Verbosity -> PackageDescription -> FilePath -> FilePath
                    -> IO ()
createZipArchive verbosity pkg tmpDir targetPref = do
    let dir       = tarBallName pkg
        zipfile   = targetPref </> dir <.> "zip"
    (zipProg, _) <- requireProgram verbosity zipProgram emptyProgramDb

    -- zip has an annoying habit of updating the target rather than creating
    -- it from scratch. While that might sound like an optimisation, it doesn't
    -- remove files already in the archive that are no longer present in the
    -- uncompressed tree.
    alreadyExists <- doesFileExist zipfile
    when alreadyExists $ removeFile zipfile

    -- We call zip with a different CWD, so have to make the path
    -- absolute. Can't just use 'canonicalizePath zipfile' since this function
    -- requires its argument to refer to an existing file.
    zipfileAbs <- fmap (</> dir <.> "zip") . canonicalizePath $ targetPref

    --TODO: use runProgramInvocation, but has to be able to set CWD
    hnd <- runProcess (programPath zipProg) ["-q", "-r", zipfileAbs, dir]
                      (Just tmpDir)
                      Nothing Nothing Nothing Nothing
    exitCode <- waitForProcess hnd
    unless (exitCode == ExitSuccess) $
      die' verbosity $ "Generating the zip file failed "
         ++ "(zip returned exit code " ++ show exitCode ++ ")"
    notice verbosity $ "Source zip archive created: " ++ zipfile
  where
    zipProgram = simpleProgram "zip"

-- | List all source files of a given add-source dependency. Exits with error if
-- something is wrong (e.g. there is no .cabal file in the given directory).
allPackageSourceFiles :: Verbosity -> SetupScriptOptions -> FilePath
                         -> IO [FilePath]
allPackageSourceFiles verbosity setupOpts0 packageDir = do
  pkg <- do
    let err = "Error reading source files of package."
    desc <- tryFindAddSourcePackageDesc verbosity packageDir err
    flattenPackageDescription `fmap` readGenericPackageDescription verbosity desc
  globalTmp <- getTemporaryDirectory
  withTempDirectory verbosity globalTmp "cabal-list-sources." $ \tempDir -> do
  let file      = tempDir </> "cabal-sdist-list-sources"
      flags     = defaultSDistFlags {
        sDistVerbosity   = Flag $ if verbosity == normal
                                  then lessVerbose verbosity else verbosity,
        sDistListSources = Flag file
        }
      setupOpts = setupOpts0 {
        -- 'sdist --list-sources' was introduced in Cabal 1.18.
        useCabalVersion = intersectVersionRanges
                            (orLaterVersion $ mkVersion [1,18,0])
                            (useCabalVersion setupOpts0),
        useWorkingDir = Just packageDir
        }

      doListSources :: IO [FilePath]
      doListSources = do
        setupWrapper verbosity setupOpts (Just pkg) sdistCommand (const flags) []
        fmap lines . readFile $ file

      onFailedListSources :: IOException -> IO ()
      onFailedListSources e = do
        warn verbosity $
          "Could not list sources of the package '"
          ++ display (packageName pkg) ++ "'."
        warn verbosity $
          "Exception was: " ++ show e

  -- Run setup sdist --list-sources=TMPFILE
  r <- doListSources `catchIO` (\e -> onFailedListSources e >> return [])
  -- Ensure that we've closed the 'readFile' handle before we exit the
  -- temporary directory.
  _ <- evaluate (length r)
  return r

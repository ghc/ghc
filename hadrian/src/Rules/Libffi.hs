module Rules.Libffi (libffiRules, libffiDependencies) where

import Hadrian.Utilities

import Packages
import Settings.Builders.Common
import Target
import Utilities

import Data.List (partition)

{-
Note [Hadrian: install libffi hack]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

There are 2 important steps in handling libffi's .a and .so files:

  1. libffi's .a and .so files are copied from the libffi build dir to the rts
  build dir. This is because libffi is ultimately bundled with the rts package.
  Relevant code is in libffiRules.
  2. The rts is "installed" via the hadrian/src/Hadrian/Haskell/Cabal/Parse.hs:
  copyPackage action. This uses the "cabal copy" command which (among other
  things) attempts to copy the bundled .a and .so|.dynlib|.dll files to the
  install dir.

There is an issue in step 1. that the name of the shared library files is not
know untill after libffi is built. As a workaround, the rts package needs just
the libffiDependencies, and the corresponding rule (defined below in
libffiRules) does the extra work of installing the shared library files into the
rts build directory after building libffi.
-}

libffiDependencies :: [FilePath]
libffiDependencies = ["ffi.h", "ffitarget.h"]

libffiStaticLibrary :: FilePath
libffiStaticLibrary = "inst/lib/libffi.a"

-- | Given a way, this return the path to the .a or .so libffi library file.
-- after building libffi, the .a and .so files will be copied to these paths.
-- These paths will be under the rts build directory as libffi is bundled with
-- the rts package.
rtsLibffiLibrary :: Stage -> Way -> Action FilePath
rtsLibffiLibrary stage way = do
    name    <- libffiLibraryName
    suf     <- libsuf way
    rtsPath <- rtsBuildPath stage
    return $ rtsPath -/- "lib" ++ name ++ suf

fixLibffiMakefile :: FilePath -> String -> String
fixLibffiMakefile top =
      replace "-MD" "-MMD"
    . replace "@toolexeclibdir@" "$(libdir)"
    . replace "@INSTALL@" ("$(subst ../install-sh," ++ top ++ "/install-sh,@INSTALL@)")

-- TODO: check code duplication w.r.t. ConfCcArgs
configureEnvironment :: Stage -> Action [CmdOption]
configureEnvironment stage = do
    cFlags  <- interpretInContext (libffiContext stage) $ mconcat
               [ cArgs
               , getStagedSettingList ConfCcArgs ]
    ldFlags <- interpretInContext (libffiContext stage) ldArgs
    sequence [ builderEnvironment "CC" $ Cc CompileC stage
             , builderEnvironment "CXX" $ Cc CompileC stage
             , builderEnvironment "LD" (Ld stage)
             , builderEnvironment "AR" (Ar Unpack stage)
             , builderEnvironment "NM" Nm
             , builderEnvironment "RANLIB" Ranlib
             , return . AddEnv  "CFLAGS" $ unwords  cFlags ++ " -w"
             , return . AddEnv "LDFLAGS" $ unwords ldFlags ++ " -w" ]

libffiRules :: Rules ()
libffiRules = forM_ [Stage1 ..] $ \stage -> do
    root       <- buildRootRules
    let libffiPathParent = root -/- stageString stage -/- "libffi"
        libffiPath       = libffiPathParent -/- "build"

    let
      -- Installs libffi into the rts build path.
      installLibffiRts = do
        useSystemFfi <- flag UseSystemFfi
        rtsPath      <- rtsBuildPath stage
        if useSystemFfi
        then do
            -- If using the system libffi, then copy the system header files.
            ffiIncludeDir <- setting FfiIncludeDir
            putBuild "| System supplied FFI library will be used"
            forM_ libffiDependencies $ \file ->
                copyFile (ffiIncludeDir -/- file) (rtsPath -/- file)
            putSuccess "| Successfully copied system FFI library header files"
        else do
            -- Build libffi.
            buildLibffi

            -- Copy header files.
            headers <- getDirectoryFiles "" [libffiPath -/- "inst/include/*"]
            forM_ headers $ \header ->
                copyFile header (rtsPath -/- takeFileName header)

            -- Find ways.
            ways <- nubOrd <$> interpretInContext
                                    (libffiContext stage)
                                    (getLibraryWays <> getRtsWays)
            let (dynamicWays, staticWays) = partition (wayUnit Dynamic) ways

            -- Install static libraries.
            forM_ staticWays $ \way -> do
                rtsLib <- rtsLibffiLibrary stage way
                copyFileUntracked (libffiPath -/- libffiStaticLibrary) rtsLib

            -- Install dynamic libraries.
            when (not $ null dynamicWays) $ do
                -- Find dynamic libraries.
                windows <- windowsHost
                osx     <- osxHost
                (dynLibsSrcDir, dynLibFiles) <- if windows
                    then do
                        let libffiName = if windows then "ffi-6" else "ffi"
                            libffiDll = "lib" ++ libffiName ++ ".dll"
                        return (libffiPath -/- "inst/bin", [libffiDll])
                    else do
                        let libffiLibPath = libffiPath -/- "inst/lib"
                        dynLibsRelative <- getDirectoryFiles
                            libffiLibPath
                            (if osx
                                then ["libffi.dylib*"]
                                else ["libffi.so*"])
                        return (libffiLibPath, dynLibsRelative)

                -- Install dynamic libraries.
                rtsPath <- rtsBuildPath stage
                forM_ dynLibFiles $ \dynLibFile -> copyFileUntracked
                    (dynLibsSrcDir -/- dynLibFile)
                    (rtsPath       -/- dynLibFile)

            putSuccess "| Successfully bundled custom library 'libffi' with rts"

      -- Extract the libffi tar file and build.
      buildLibffi = do
        -- Clear build directory.
        removeDirectory libffiPath
        createDirectory libffiPathParent

        -- Extract the libffi tar arcive.
        tarball <- unifyPath . fromSingleton "Exactly one LibFFI tarball is expected"
               <$> getDirectoryFiles "" ["libffi-tarballs/libffi*.tar.gz"]
        need [tarball]
        -- Go from 'libffi-3.99999+git20171002+77e130c.tar.gz' to 'libffi-3.99999'
        let libname = takeWhile (/= '+') $ takeFileName tarball
            extractParentDir = root -/- stageString stage
            extractDir = extractParentDir -/- libname
        removeDirectory extractDir
        actionFinally
            (do
                build $ target (libffiContext stage) (Tar Extract)
                                                  [tarball]
                                                  [extractParentDir]
                moveDirectory extractDir libffiPath
            )
            (removeFiles extractDir [".//*"])

        -- Fix Makefile.in.
        let mk   = libffiPath -/- "Makefile"
            mkIn = mk <.> "in"
        top <- topDirectory
        fixFile mkIn (fixLibffiMakefile top)

        -- Configure.
        forM_ ["config.guess", "config.sub"] $ \file -> do
            copyFile file (libffiPath -/- file)
        env <- configureEnvironment stage
        buildWithCmdOptions env $
          target (libffiContext stage) (Configure libffiPath) [mkIn] [mk]

        -- Build.
        build $ target (libffiContext stage) (Make libffiPath) [] []
        putSuccess "| Successfully built custom library 'libffi'"

    -- See [Hadrian: install libffi hack]. Needing any of the libffi header
    -- files also results in installing relevant libffi library files to rtsPath
    -- if necessary. Instead, the rts should need and copy the relevant libffi
    -- library files. The problem is that the names of the library files are
    -- only known after building libffi.
    fmap ((root -/- stageString stage -/- "rts/build") -/-) libffiDependencies
      &%> \_ -> installLibffiRts

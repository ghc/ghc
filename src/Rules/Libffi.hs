module Rules.Libffi (libffiRules, libffiBuildPath, libffiDependencies) where

import Hadrian.Utilities

import Settings.Builders.Common
import Settings.Packages.Rts
import Target
import Utilities

-- | Libffi is considered a Stage1 package. This determines its build directory.
libffiContext :: Context
libffiContext = vanillaContext Stage1 libffi

-- | Build directory for in-tree Libffi library.
libffiBuildPath :: Action FilePath
libffiBuildPath = buildPath libffiContext

libffiDependencies :: [FilePath]
libffiDependencies = ["ffi.h", "ffitarget.h"]

libffiLibrary :: FilePath
libffiLibrary = "inst/lib/libffi.a"

fixLibffiMakefile :: FilePath -> String -> String
fixLibffiMakefile top =
      replace "-MD" "-MMD"
    . replace "@toolexeclibdir@" "$(libdir)"
    . replace "@INSTALL@" ("$(subst ../install-sh," ++ top ++ "/install-sh,@INSTALL@)")

-- TODO: remove code duplication (see Settings/Builders/GhcCabal.hs)
-- TODO: check code duplication w.r.t. ConfCcArgs
configureEnvironment :: Action [CmdOption]
configureEnvironment = do
    cFlags  <- interpretInContext libffiContext $ mconcat
               [ cArgs
               , getStagedSettingList ConfCcArgs ]
    ldFlags <- interpretInContext libffiContext ldArgs
    sequence [ builderEnvironment "CC" $ Cc CompileC Stage1
             , builderEnvironment "CXX" $ Cc CompileC Stage1
             , builderEnvironment "LD" Ld
             , builderEnvironment "AR" (Ar Unpack Stage1)
             , builderEnvironment "NM" Nm
             , builderEnvironment "RANLIB" Ranlib
             , return . AddEnv  "CFLAGS" $ unwords  cFlags ++ " -w"
             , return . AddEnv "LDFLAGS" $ unwords ldFlags ++ " -w" ]

libffiRules :: Rules ()
libffiRules = do
    fmap ("//rts" -/-) libffiDependencies &%> \_ -> do
        libffiPath <- libffiBuildPath
        need [libffiPath -/- libffiLibrary]

    "//" ++ libffiLibrary %> \_ -> do
        useSystemFfi <- flag UseSystemFfi
        rtsPath      <- rtsBuildPath
        if useSystemFfi
        then do
            ffiIncludeDir <- setting FfiIncludeDir
            putBuild "| System supplied FFI library will be used"
            forM_ ["ffi.h", "ffitarget.h"] $ \file ->
                copyFile (ffiIncludeDir -/- file) (rtsPath -/- file)
            putSuccess "| Successfully copied system FFI library header files"
        else do
            libffiPath <- libffiBuildPath
            build $ target libffiContext (Make libffiPath) [] []

            hs <- getDirectoryFiles "" [libffiPath -/- "inst/include/*"]
            forM_ hs $ \header ->
                copyFile header (rtsPath -/- takeFileName header)

            ways <- interpretInContext libffiContext (getLibraryWays <> getRtsWays)
            forM_ (nubOrd ways) $ \way -> do
                rtsLib <- rtsLibffiLibrary way
                copyFileUntracked (libffiPath -/- libffiLibrary) rtsLib

            putSuccess "| Successfully built custom library 'libffi'"

    "//libffi/Makefile.in" %> \mkIn -> do
        libffiPath <- libffiBuildPath
        removeDirectory libffiPath
        tarball <- unifyPath . fromSingleton "Exactly one LibFFI tarball is expected"
               <$> getDirectoryFiles "" ["libffi-tarballs/libffi*.tar.gz"]

        need [tarball]
        -- Go from 'libffi-3.99999+git20171002+77e130c.tar.gz' to 'libffi-3.99999'
        let libname = takeWhile (/= '+') $ takeFileName tarball

        root <- buildRoot
        removeDirectory (root -/- libname)
        -- TODO: Simplify.
        actionFinally (do
            build $ target libffiContext (Tar Extract) [tarball] [root]
            moveDirectory (root -/- libname) libffiPath) $
                removeFiles root [libname <//> "*"]

        top <- topDirectory
        fixFile mkIn (fixLibffiMakefile top)

    -- TODO: Get rid of hard-coded @libffi@.
    "//libffi/Makefile" %> \mk -> do
        need [mk <.> "in"]
        libffiPath <- libffiBuildPath
        forM_ ["config.guess", "config.sub"] $ \file ->
            copyFile file (libffiPath -/- file)

        env <- configureEnvironment
        buildWithCmdOptions env $
            target libffiContext (Configure libffiPath) [mk <.> "in"] [mk]

{-# LANGUAGE TypeFamilies #-}

module Rules.Libffi (
    LibffiDynLibs(..),
    needLibffi, askLibffilDynLibs, libffiRules, libffiLibrary, libffiHeaderFiles,
    libffiHeaders, libffiSystemHeaders, libffiName
    ) where

import Hadrian.Utilities

import Packages
import Settings.Builders.Common
import Target
import Utilities

{- Note [Libffi indicating inputs]

First see https://gitlab.haskell.org/ghc/ghc/wikis/Developing-Hadrian for an
explanation of "indicating input". Part of the definition is copied here for
your convenience:

    change in the vital output -> change in the indicating inputs

In the case of building libffi `vital output = built libffi library files` and
we can consider the libffi archive file (i.e. the "libffi-tarballs/libffi*.tar.gz"
file) to be the only indicating input besides the build tools (e.g. make).
Note building libffi is split into a few rules, but we also expect that:

    no change in the archive file -> no change in the intermediate build artifacts

and so the archive file is still a valid choice of indicating input for
all libffi rules. Hence we can get away with `need`ing only the archive file and
don't have to `need` intermediate build artifacts (besides those to trigger
dependant libffi rules i.e. to generate vital inputs as is noted on the wiki).
It is then safe to `trackAllow` the libffi build directory as is done in
`needLibfffiArchive`.

A disadvantage to this approach is that changing the archive file forces a clean
build of libffi i.e. we cannot incrementally build libffi. This seems like a
performance issue, but is justified as building libffi is fast and the archive
file is rarely changed.

-}

-- | Oracle question type. The oracle returns the list of dynamic
-- libffi library file paths (all but one of which should be symlinks).
newtype LibffiDynLibs = LibffiDynLibs Stage
        deriving (Eq, Show, Hashable, Binary, NFData)
type instance RuleResult LibffiDynLibs = [FilePath]

askLibffilDynLibs :: Stage -> Action [FilePath]
askLibffilDynLibs stage = askOracle (LibffiDynLibs stage)

-- | The path to the dynamic library manifest file. The file contains all file
-- paths to libffi dynamic library file paths.
-- The path is calculated but not `need`ed.
dynLibManifest' :: Monad m => m FilePath -> Stage -> m FilePath
dynLibManifest' getRoot stage = do
    root <- getRoot
    return $ root -/- stageString stage -/- pkgName libffi -/- ".dynamiclibs"

dynLibManifestRules :: Stage -> Rules FilePath
dynLibManifestRules = dynLibManifest' buildRootRules

dynLibManifest :: Stage -> Action FilePath
dynLibManifest = dynLibManifest' buildRoot

-- | Need the (locally built) libffi library.
needLibffi :: Stage -> Action ()
needLibffi stage = do
    manifest <- dynLibManifest stage
    need [manifest]

-- | Context for @libffi@.
libffiContext :: Stage -> Action Context
libffiContext stage = do
    ways <- interpretInContext
            (Context stage libffi (error "libffiContext: way not set"))
            getLibraryWays
    return . Context stage libffi $ if any (wayUnit Dynamic) ways
        then dynamic
        else vanilla

-- | The name of the (locally built) library
libffiName :: Expr String
libffiName = do
    way <- getWay
    return $ libffiName' (Dynamic `wayUnit` way)

-- | The name of the (locally built) library
libffiName' :: Bool -> String
libffiName' dynamic = (if dynamic     then ""      else "C")
                   ++ (if windowsHost then "ffi-6" else "ffi")

libffiLibrary :: FilePath
libffiLibrary = "inst/lib/libffi.a"

libffiHeaderFiles :: [FilePath]
libffiHeaderFiles = ["ffi.h", "ffitarget.h"]

libffiHeaders :: Stage -> Action [FilePath]
libffiHeaders stage = do
    path <- libffiBuildPath stage
    return $ fmap ((path -/- "inst/include") -/-) libffiHeaderFiles

libffiSystemHeaders :: Action [FilePath]
libffiSystemHeaders = do
    ffiIncludeDir <- setting FfiIncludeDir
    return $ fmap (ffiIncludeDir -/-) libffiHeaderFiles

fixLibffiMakefile :: FilePath -> String -> String
fixLibffiMakefile top =
      replace "-MD" "-MMD"
    . replace "@toolexeclibdir@" "$(libdir)"
    . replace "@INSTALL@" ("$(subst ../install-sh," ++ top ++ "/install-sh,@INSTALL@)")

-- TODO: check code duplication w.r.t. ConfCcArgs
configureEnvironment :: Stage -> Action [CmdOption]
configureEnvironment stage = do
    context <- libffiContext stage
    cFlags  <- interpretInContext context $ mconcat
               [ cArgs
               , getStagedSettingList ConfCcArgs ]
    ldFlags <- interpretInContext context ldArgs
    sequence [ builderEnvironment "CC" $ Cc CompileC stage
             , builderEnvironment "CXX" $ Cc CompileC stage
             , builderEnvironment "LD" (Ld stage)
             , builderEnvironment "AR" (Ar Unpack stage)
             , builderEnvironment "NM" Nm
             , builderEnvironment "RANLIB" Ranlib
             , return . AddEnv  "CFLAGS" $ unwords  cFlags ++ " -w"
             , return . AddEnv "LDFLAGS" $ unwords ldFlags ++ " -w" ]

-- Need the libffi archive and `trackAllow` all files in the build directory.
-- See [Libffi indicating inputs].
needLibfffiArchive :: FilePath -> Action FilePath
needLibfffiArchive buildPath = do
    top <- topDirectory
    tarball <- unifyPath
                . fromSingleton "Exactly one LibFFI tarball is expected"
                <$> getDirectoryFiles top ["libffi-tarballs/libffi*.tar.gz"]
    need [top -/- tarball]
    trackAllow [buildPath -/- "**"]
    return tarball

libffiRules :: Rules ()
libffiRules = do
  _ <- addOracleCache $ \ (LibffiDynLibs stage)
                         -> readFileLines =<< dynLibManifest stage
  forM_ [Stage1 ..] $ \stage -> do
    root <- buildRootRules
    let path       = root -/- stageString stage
        libffiPath = path -/- pkgName libffi -/- "build"

    -- We set a higher priority because this rule overlaps with the build rule
    -- for static libraries 'Rules.Library.libraryRules'.
    dynLibMan <- dynLibManifestRules stage
    let topLevelTargets =  [ libffiPath -/- libffiLibrary
                           , dynLibMan
                           ]
    priority 2 $ topLevelTargets &%> \_ -> do
        _ <- needLibfffiArchive libffiPath
        context <- libffiContext stage

        -- Note this build needs the Makefile, triggering the rules bellow.
        build $ target context (Make libffiPath) [] []

        -- Produces all install files.
        produces =<< (\\ topLevelTargets)
                 <$> liftIO (getDirectoryFilesIO "." [libffiPath -/- "inst//*"])

        -- Find dynamic libraries.
        dynLibFiles <- do
            let libfilesDir = libffiPath -/-
                    (if windowsHost then "inst" -/- "bin" else "inst" -/- "lib")
                libffiName'' = libffiName' True
                dynlibext
                    | windowsHost = "dll"
                    | osxHost     = "dylib"
                    | otherwise   = "so"
                filepat = "lib" ++ libffiName'' ++ "." ++ dynlibext ++ "*"
            liftIO $ getDirectoryFilesIO "." [libfilesDir -/- filepat]

        writeFileLines dynLibMan dynLibFiles
        putSuccess "| Successfully build libffi."

    fmap (libffiPath -/-) ["Makefile.in", "configure" ] &%> \[mkIn, _] -> do
        -- Extract libffi tar file
        context <- libffiContext stage
        removeDirectory libffiPath
        tarball <- needLibfffiArchive libffiPath
        -- Go from 'libffi-3.99999+git20171002+77e130c.tar.gz' to 'libffi-3.99999'
        let libname = takeWhile (/= '+') $ takeFileName tarball

        -- Move extracted directory to libffiPath.
        root <- buildRoot
        removeDirectory (root -/- libname)
        actionFinally (do
            build $ target context (Tar Extract) [tarball] [path]
            moveDirectory (path -/- libname) libffiPath) $
            -- And finally:
            removeFiles (path) [libname -/- "**"]

        top <- topDirectory
        fixFile mkIn (fixLibffiMakefile top)

        files <- liftIO $ getDirectoryFilesIO "." [libffiPath -/- "**"]
        produces files

    fmap (libffiPath -/-) ["Makefile", "config.guess", "config.sub"] &%> \[mk, _, _] -> do
        _ <- needLibfffiArchive libffiPath
        context <- libffiContext stage

        -- This need rule extracts the libffi tar file to libffiPath.
        need [mk <.> "in"]

        -- Configure.
        forM_ ["config.guess", "config.sub"] $ \file -> do
            copyFile file (libffiPath -/- file)
        env <- configureEnvironment stage
        buildWithCmdOptions env $
            target context (Configure libffiPath) [mk <.> "in"] [mk]

        dir   <- setting BuildPlatform
        files <- liftIO $ getDirectoryFilesIO "." [libffiPath -/- dir -/- "**"]
        produces files

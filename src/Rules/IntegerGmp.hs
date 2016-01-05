module Rules.IntegerGmp (integerGmpRules, integerGmpObjects, integerGmpLibraryH) where

import Base
import Expression
import GHC
import Oracles.Config.Setting
import Rules.Actions
import Settings.User
import Settings.TargetDirectory

integerGmpBase :: FilePath
integerGmpBase = "libraries/integer-gmp/gmp"

integerGmpBuild :: FilePath
integerGmpBuild = integerGmpBase -/- "gmpbuild"

integerGmpObjects :: FilePath
integerGmpObjects = integerGmpBase -/- "objs"

integerGmpLibrary :: FilePath
integerGmpLibrary = integerGmpBase -/- "libgmp.a"

integerGmpLibraryInTreeH :: FilePath
integerGmpLibraryInTreeH = integerGmpBase -/- "gmp.h"

integerGmpLibraryH :: FilePath
integerGmpLibraryH = pkgPath integerGmp -/- "include/ghc-gmp.h"

integerGmpLibraryFakeH :: FilePath
integerGmpLibraryFakeH = integerGmpBase -/- "ghc-gmp.h"

-- relative to integerGmpBuild
integerGmpPatch :: FilePath
integerGmpPatch = ".." -/- "tarball" -/- "gmp-5.0.4.patch"

target :: PartialTarget
target = PartialTarget Stage0 integerGmp

-- TODO: See Libffi.hs about removing code duplication.
configureEnvironment :: Action [CmdOption]
configureEnvironment = do
    sequence [ builderEnv "CC" $ Gcc Stage1
             , builderEnv "AR" Ar
             , builderEnv "NM" Nm ]
  where
    builderEnv var builder = do
        needBuilder False builder
        path <- builderPath builder
        return $ AddEnv var path

configureArguments :: Action [String]
configureArguments = do
    hostPlatform  <- setting HostPlatform
    buildPlatform <- setting BuildPlatform
    return [ "--enable-shared=no"
           , "--host=" ++ hostPlatform
           , "--build=" ++ buildPlatform]

-- TODO: we rebuild integer-gmp every time.
integerGmpRules :: Rules ()
integerGmpRules = do

    -- TODO: split into multiple rules
    integerGmpLibraryH %> \_ -> do
        when trackBuildSystem $ need [sourcePath -/- "Rules/IntegerGmp.hs"]

        -- remove the old build folder, if it exists.
        liftIO $ removeFiles integerGmpBuild ["//*"]
        liftIO $ removeFiles (integerGmpObjects) ["//*"]

        -- unpack the gmp tarball.
        -- Note: We use a tarball like gmp-4.2.4-nodoc.tar.bz2, which is
        -- gmp-4.2.4.tar.bz2 repacked without the doc/ directory contents.
        -- That's because the doc/ directory contents are under the GFDL,
        -- which causes problems for Debian.
        tarballs <- getDirectoryFiles "" [integerGmpBase -/- "tarball/gmp*.tar.bz2"]
        when (length tarballs /= 1) $
            putError $ "integerGmpRules: exactly one tarball expected"
                     ++ "(found: " ++ show tarballs ++ ")."

        need tarballs
        build $ fullTarget target Tar tarballs [integerGmpBase]

        -- move gmp-<version> to gmpbuild
        let filename = dropExtension . dropExtension . takeFileName $ head tarballs
            suffix   = "-nodoc-patched"
        unless (suffix `isSuffixOf` filename) $
            putError $ "integerGmpRules: expected suffix " ++ suffix
                     ++ " (found: " ++ filename ++ ")."
        let libname = take (length filename - length suffix) filename
        moveDirectory (integerGmpBase -/- libname) integerGmpBuild

        -- apply patches
        -- TODO: replace "patch" with PATCH_CMD
        unit . quietly $ cmd Shell (EchoStdout False) [Cwd integerGmpBase] "patch -p0 < gmpsrc.patch"
        putBuild $ "| Apply " ++ (integerGmpBase -/- "gmpsrc.patch")
        unit . quietly $ cmd Shell (EchoStdout False) [Cwd integerGmpBuild] "patch -p1 < " [integerGmpPatch]
        putBuild $ "| Apply " ++ (integerGmpBase -/- integerGmpPatch)

        -- TODO: What's `chmod +x libraries/integer-gmp/gmp/ln` for?

        envs <- configureEnvironment
        args <- configureArguments
        runConfigure integerGmpBuild envs args

        -- check whether we need to build in tree gmp
        -- this is indicated by line "HaveFrameworkGMP = YES" in `config.mk`
        need [pkgDataFile Stage1 integerGmp]
        configMk <- liftIO . readFile $ integerGmpBase -/- "config.mk"
        if "HaveFrameworkGMP = YES" `isInfixOf` configMk
        then do
            putBuild "\n| GMP framework detected and will be used"
            copyFile integerGmpLibraryFakeH integerGmpLibraryH
        else do
            putBuild "\n| No GMP framework detected"
            runMake integerGmpBuild []

            copyFile integerGmpLibraryInTreeH integerGmpLibraryH
            -- TODO: why copy library, can we move it instead?
            copyFile (integerGmpBuild -/- ".libs/libgmp.a") integerGmpLibrary

            createDirectory integerGmpObjects
            build $ fullTarget target Ar [integerGmpLibrary] [integerGmpObjects]

            runBuilder Ranlib [integerGmpLibrary]

        putSuccess "| Successfully built custom library 'integer-gmp'"

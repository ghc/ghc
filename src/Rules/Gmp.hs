module Rules.Gmp (
    gmpRules, gmpBuildPath, gmpObjects, gmpLibraryH, gmpDependencies
    ) where

import Base
import Expression
import GHC
import Oracles.Config.Setting
import Rules.Actions
import Settings.Packages.IntegerGmp
import Settings.User

gmpBase :: FilePath
gmpBase = "libraries/integer-gmp/gmp"

gmpTarget :: PartialTarget
gmpTarget = PartialTarget Stage0 integerGmp

gmpObjects :: FilePath
gmpObjects = gmpBuildPath -/- "objs"

gmpLibrary :: FilePath
gmpLibrary = gmpBuildPath -/- "libgmp.a"

gmpLibraryInTreeH :: FilePath
gmpLibraryInTreeH = gmpBuildPath -/- "include/gmp.h"

gmpLibraryH :: FilePath
gmpLibraryH = gmpBuildPath -/- "include/ghc-gmp.h"

gmpLibraryFakeH :: FilePath
gmpLibraryFakeH = gmpBase -/- "ghc-gmp.h"

gmpDependencies :: [FilePath]
gmpDependencies = [gmpLibraryH]

gmpPatches :: [FilePath]
gmpPatches = (gmpBase -/-) <$> ["gmpsrc.patch", "tarball/gmp-5.0.4.patch"]

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

-- TODO: we rebuild gmp every time.
gmpRules :: Rules ()
gmpRules = do

    -- TODO: split into multiple rules
    gmpLibraryH %> \_ -> do
        when trackBuildSystem $ need [sourcePath -/- "Rules/Gmp.hs"]

        -- Do we need this step?
        liftIO $ removeFiles gmpBuildPath ["//*"]

        -- Note: We use a tarball like gmp-4.2.4-nodoc.tar.bz2, which is
        -- gmp-4.2.4.tar.bz2 repacked without the doc/ directory contents.
        -- That's because the doc/ directory contents are under the GFDL,
        -- which causes problems for Debian.
        tarballs <- getDirectoryFiles "" [gmpBase -/- "tarball/gmp*.tar.bz2"]
        when (length tarballs /= 1) $
            putError $ "gmpRules: exactly one tarball expected"
                     ++ "(found: " ++ show tarballs ++ ")."

        need tarballs

        createDirectory gmpBuildPath
        build $ fullTarget gmpTarget Tar tarballs [gmpBuildPath]

        -- TODO: replace "patch" with PATCH_CMD
        forM_ gmpPatches $ \src -> do
            let patch     = takeFileName src
                patchPath = gmpBuildPath -/- patch
            copyFile src patchPath
            putBuild $ "| Apply " ++ patchPath
            unit . quietly $ cmd Shell (EchoStdout False) [Cwd gmpBuildPath] "patch -p0 <" [patch]

        -- TODO: What's `chmod +x libraries/integer-gmp/gmp/ln` for?

        let filename = dropExtension . dropExtension . takeFileName $ head tarballs
            suffix   = "-nodoc-patched"
        unless (suffix `isSuffixOf` filename) $
            putError $ "gmpRules: expected suffix " ++ suffix
                     ++ " (found: " ++ filename ++ ")."
        let libName = take (length filename - length suffix) filename
            libPath = gmpBuildPath -/- libName

        envs <- configureEnvironment
        args <- configureArguments
        runConfigure libPath envs args

        -- TODO: currently we configure integerGmp package twice -- optimise
        runConfigure (pkgPath integerGmp) [] []

        createDirectory $ takeDirectory gmpLibraryH
        -- check whether we need to build in tree gmp
        -- this is indicated by line "HaveFrameworkGMP = YES" in `config.mk`
        configMk <- liftIO . readFile $ gmpBase -/- "config.mk"
        if "HaveFrameworkGMP = YES" `isInfixOf` configMk
        then do
            putBuild "| GMP framework detected and will be used"
            copyFile gmpLibraryFakeH gmpLibraryH
        else do
            putBuild "| No GMP framework detected; in tree GMP will be built"
            runMake libPath ["MAKEFLAGS="]

            copyFile (libPath -/- "gmp.h") gmpLibraryInTreeH
            copyFile (libPath -/- "gmp.h") gmpLibraryH
            -- TODO: why copy library, can we move it instead?
            copyFile (libPath -/- ".libs/libgmp.a") gmpLibrary

            createDirectory gmpObjects
            build $ fullTarget gmpTarget Ar [gmpLibrary] [gmpObjects]

            runBuilder Ranlib [gmpLibrary]

        putSuccess "| Successfully built custom library 'integer-gmp'"

    -- gmpLibraryInTreeH %> \_ -> need [gmpLibraryH]

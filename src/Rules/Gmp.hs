module Rules.Gmp (gmpRules, gmpBuildPath, gmpObjects, gmpLibraryH) where

import Base
import Expression
import GHC
import Oracles.Config.Setting
import Rules.Actions
import Settings.Packages.IntegerGmp
import Settings.User
import Settings.Paths
import Target

gmpBase :: FilePath
gmpBase = "libraries/integer-gmp/gmp"

gmpContext :: Context
gmpContext = vanillaContext Stage1 integerGmp

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

gmpPatches :: [FilePath]
gmpPatches = (gmpBase -/-) <$> ["gmpsrc.patch", "tarball/gmp-5.0.4.patch"]

-- TODO: See Libffi.hs about removing code duplication.
configureEnvironment :: Action [CmdOption]
configureEnvironment = do
    sequence [ builderEnv "CC" $ Cc Compile Stage1
             , builderEnv "AR" Ar
             , builderEnv "NM" Nm ]
  where
    builderEnv var bld = do
        needBuilder False bld
        path <- builderPath bld
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
        liftIO $ removeFiles gmpBuildPath ["//*"]
        createDirectory $ takeDirectory gmpLibraryH

        -- We don't use system GMP on Windows. TODO: fix?
        windows  <- windowsHost
        configMk <- readFile' $ gmpBase -/- "config.mk"
        if not windows && any (`isInfixOf` configMk)
            [ "HaveFrameworkGMP = YES", "HaveLibGmp = YES" ]
        then do
            putBuild "| GMP library/framework detected and will be used"
            copyFile gmpLibraryFakeH gmpLibraryH
        else do
            putBuild "| No GMP library/framework detected; in tree GMP will be built"

            -- Note: We use a tarball like gmp-4.2.4-nodoc.tar.bz2, which is
            -- gmp-4.2.4.tar.bz2 repacked without the doc/ directory contents.
            -- That's because the doc/ directory contents are under the GFDL,
            -- which causes problems for Debian.
            tarballs <- getDirectoryFiles "" [gmpBase -/- "tarball/gmp*.tar.bz2"]
            when (length tarballs /= 1) $
                putError $ "gmpRules: exactly one tarball expected"
                         ++ "(found: " ++ show tarballs ++ ")."

            need tarballs
            build $ Target gmpContext Tar tarballs [gmpBuildPath]

            forM_ gmpPatches $ \src -> do
                let patch     = takeFileName src
                    patchPath = gmpBuildPath -/- patch
                copyFile src patchPath
                applyPatch gmpBuildPath patch

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

            runMake libPath ["MAKEFLAGS="]

            copyFile (libPath -/- "gmp.h") gmpLibraryInTreeH
            copyFile (libPath -/- "gmp.h") gmpLibraryH
            -- TODO: why copy library, can we move it instead?
            copyFile (libPath -/- ".libs/libgmp.a") gmpLibrary

            createDirectory gmpObjects
            build $ Target gmpContext Ar [gmpLibrary] [gmpObjects]

            runBuilder Ranlib [gmpLibrary]

        putSuccess "| Successfully built custom library 'gmp'"

    gmpLibraryInTreeH %> \_ -> need [gmpLibraryH]

    -- This causes integerGmp package to be configured, hence creating the files
    [gmpBase -/- "config.mk", gmpBuildInfoPath] &%> \_ ->
        need [pkgDataFile gmpContext]

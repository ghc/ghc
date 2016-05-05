module Rules.Gmp (gmpRules) where

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

gmpLibraryInTreeH :: FilePath
gmpLibraryInTreeH = gmpBuildPath -/- "include/gmp.h"

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
        needBuilder bld
        path <- builderPath bld
        return $ AddEnv var path

-- TODO: we rebuild gmp every time.
gmpRules :: Rules ()
gmpRules = do
    -- TODO: split into multiple rules
    gmpLibraryH %> \_ -> do
        when trackBuildSystem $ need [sourcePath -/- "Rules/Gmp.hs"]
        removeDirectory gmpBuildPath

        -- We don't use system GMP on Windows. TODO: fix?
        windows  <- windowsHost
        configMk <- readFile' $ gmpBase -/- "config.mk"
        if not windows && any (`isInfixOf` configMk)
            [ "HaveFrameworkGMP = YES", "HaveLibGmp = YES" ]
        then do
            putBuild "| GMP library/framework detected and will be used"
            createDirectory $ takeDirectory gmpLibraryH
            copyFile gmpLibraryFakeH gmpLibraryH
        else do
            putBuild "| No GMP library/framework detected; in tree GMP will be built"

            -- Note: We use a tarball like gmp-4.2.4-nodoc.tar.bz2, which is
            -- gmp-4.2.4.tar.bz2 repacked without the doc/ directory contents.
            -- That's because the doc/ directory contents are under the GFDL,
            -- which causes problems for Debian.
            tarballs <- getDirectoryFiles "" [gmpBase -/- "tarball/gmp*.tar.bz2"]
            tarball  <- case tarballs of
                [file] -> return $ unifyPath file
                _      -> putError $ "gmpRules: exactly one tarball expected"
                          ++ "(found: " ++ show tarballs ++ ")."

            withTempDir $ \dir -> do
                let tmp = unifyPath dir
                need [tarball]
                build $ Target gmpContext Tar [tarball] [tmp]

                forM_ gmpPatches $ \src -> do
                    let patch     = takeFileName src
                        patchPath = tmp -/- patch
                    copyFile src patchPath
                    applyPatch tmp patch

                let name = dropExtension . dropExtension $ takeFileName tarball
                libName <- case stripSuffix "-nodoc-patched" name of
                    Just rest -> return rest
                    Nothing   -> putError $ "gmpRules: expected suffix "
                        ++ "-nodoc-patched (found: " ++ name ++ ")."

                moveDirectory (tmp -/- libName) gmpBuildPath

            env <- configureEnvironment
            buildWithCmdOptions env $
                Target gmpContext (Configure gmpBuildPath)
                       [gmpBuildPath -/- "Makefile.in"]
                       [gmpBuildPath -/- "Makefile"]

            runMake gmpBuildPath ["MAKEFLAGS="]

            createDirectory $ takeDirectory gmpLibraryH
            copyFile (gmpBuildPath -/- "gmp.h") gmpLibraryH
            copyFile (gmpBuildPath -/- "gmp.h") gmpLibraryInTreeH
            moveFile (gmpBuildPath -/- ".libs/libgmp.a") gmpLibrary

            createDirectory gmpObjects
            build $ Target gmpContext Ar [gmpLibrary] [gmpObjects]

            runBuilder Ranlib [gmpLibrary]

        putSuccess "| Successfully built custom library 'gmp'"

    gmpLibraryInTreeH %> \_ -> need [gmpLibraryH]

    -- This causes integerGmp package to be configured, hence creating the files
    [gmpBase -/- "config.mk", gmpBuildInfoPath] &%> \_ ->
        need [pkgDataFile gmpContext]

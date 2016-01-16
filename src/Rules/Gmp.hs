module Rules.Gmp (
    gmpRules, gmpBuildPath, gmpObjects, gmpLibraryH, gmpDependencies
    ) where

import qualified System.Directory as IO

import Base
import Expression
import GHC
import Oracles.Config.Setting
import Rules.Actions
import Settings.Builders.Ghc
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

configureIntGmpArguments :: Action [String]
configureIntGmpArguments = do
    includes      <- settingList GmpIncludeDirs
    libs          <- settingList GmpLibDirs
    return $ map ("--with-gmp-includes=" ++) includes
          ++ map ("--with-gmp-libraries=" ++) libs

-- TODO: we rebuild gmp every time.
gmpRules :: Rules ()
gmpRules = do

    -- TODO: split into multiple rules
    [gmpLibraryH, gmpLibNameCache] &%> \_ -> do
        when trackBuildSystem $ need [sourcePath -/- "Rules/Gmp.hs"]

        liftIO $ removeFiles gmpBuildPath ["//*"]

        envs <- configureEnvironment
        -- TODO: without the optimisation below we configure integerGmp package
        -- twice -- think how this can be optimised (shall we solve #18 first?)
        -- TODO: this is a hacky optimisation: we do not rerun configure of
        -- integerGmp package if we detect the results of the previous run
        unlessM (liftIO . IO.doesFileExist $ gmpBase -/- "config.mk") $ do
            args <- configureIntGmpArguments
            runConfigure (pkgPath integerGmp) envs args

        createDirectory $ takeDirectory gmpLibraryH
        -- We don't use system GMP on Windows. TODO: fix?
        -- TODO: we do not track "config.mk" and "integer-gmp.buildinfo", see #173
        windows <- windowsHost
        configMk <- liftIO . readFile $ gmpBase -/- "config.mk"
        if not windows && any (`isInfixOf` configMk) [ "HaveFrameworkGMP = YES", "HaveLibGmp = YES" ]
        then do
            putBuild "| GMP library/framework detected and will be used"
            copyFile gmpLibraryFakeH gmpLibraryH
            buildInfo <- liftIO . readFile $ pkgPath integerGmp -/- "integer-gmp.buildinfo"
            let prefix = "extra-libraries: "
                libs s = case stripPrefix prefix s of
                    Nothing    -> []
                    Just value -> words value
            writeFileChanged gmpLibNameCache . unlines . concatMap libs $ lines buildInfo
        else do
            putBuild "| No GMP library/framework detected; in tree GMP will be built"
            writeFileChanged gmpLibNameCache ""

            -- Note: We use a tarball like gmp-4.2.4-nodoc.tar.bz2, which is
            -- gmp-4.2.4.tar.bz2 repacked without the doc/ directory contents.
            -- That's because the doc/ directory contents are under the GFDL,
            -- which causes problems for Debian.
            tarballs <- getDirectoryFiles "" [gmpBase -/- "tarball/gmp*.tar.bz2"]
            when (length tarballs /= 1) $
                putError $ "gmpRules: exactly one tarball expected"
                         ++ "(found: " ++ show tarballs ++ ")."

            need tarballs
            build $ fullTarget gmpTarget Tar tarballs [gmpBuildPath]

            forM_ gmpPatches $ \src -> do
                let patch     = takeFileName src
                    patchPath = gmpBuildPath -/- patch
                copyFile src patchPath
                applyPatch gmpBuildPath patch

            -- TODO: What's `chmod +x libraries/integer-gmp/gmp/ln` for?

            let filename = dropExtension . dropExtension . takeFileName $ head tarballs
                suffix   = "-nodoc-patched"
            unless (suffix `isSuffixOf` filename) $
                putError $ "gmpRules: expected suffix " ++ suffix
                         ++ " (found: " ++ filename ++ ")."
            let libName = take (length filename - length suffix) filename
                libPath = gmpBuildPath -/- libName

            args2 <- configureArguments
            runConfigure libPath envs args2

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

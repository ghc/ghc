module Rules.IntegerGmp (integerGmpRules, integerGmpLibrary) where

import System.Directory

import Base
import Expression
import GHC
import Oracles.Config.Setting
import Rules.Actions

integerGmpBase :: FilePath
integerGmpBase = "libraries" -/- "integer-gmp" -/- "gmp"

integerGmpBuild :: FilePath
integerGmpBuild = integerGmpBase -/- "gmpbuild"

integerGmpLibrary :: FilePath
integerGmpLibrary = integerGmpBase -/- "libgmp.a"

-- relative to integerGmpBuild
integerGmpPatch :: FilePath
integerGmpPatch = ".." -/- "tarball" -/- "gmp-5.0.4.patch"

target :: PartialTarget
target = PartialTarget Stage0 integerGmp

-- TODO: See Libffi.hs about removing code duplication.
configureEnvironment :: Action [CmdOption]
configureEnvironment = do
    sequence [ builderEnv "CC" $ Gcc Stage1
             , builderEnv "CXX" $ Gcc Stage1
             , builderEnv "AR" Ar
             , builderEnv "NM" Nm]
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
    integerGmpLibrary %> \_ -> do
        need [sourcePath -/- "Rules" -/- "integerGmp.hs"]

        -- remove the old build folder, if it exists.
        liftIO $ removeFiles integerGmpBuild ["//*"]

        -- unpack the gmp tarball.
        -- Note: We use a tarball like gmp-4.2.4-nodoc.tar.bz2, which is
        -- gmp-4.2.4.tar.bz2 repacked without the doc/ directory contents.
        -- That's because the doc/ directory contents are under the GFDL,
        -- which causes problems for Debian.
        tarballs <- getDirectoryFiles "" [integerGmpBase -/- "tarball/gmp*.tar.bz2"]
        when (length tarballs /= 1) $
            putError $ "integerGmpRules: exactly one tarball expected"
                     ++ "(found: " ++ show tarballs ++ ")."
        let filename = dropExtension . dropExtension . takeFileName $ head tarballs
        let suffix = "-nodoc-patched"
        unless (suffix `isSuffixOf` filename) $
            putError $ "integerGmpRules: expected suffix " ++ suffix
                     ++ " (found: " ++ filename ++ ")."
        let libname = take (length filename - length suffix) filename

        need tarballs
        build $ fullTarget target Tar tarballs [integerGmpBase]

        -- move gmp-<version> to gmpbuild
        let integerGmpExtracted = integerGmpBase -/- libname
        liftIO $ renameDirectory integerGmpExtracted integerGmpBuild
        putBuild $ "| Move " ++ integerGmpExtracted ++ " -> " ++ integerGmpBuild

        -- apply patches
        -- TODO: replace "patch" with PATCH_CMD
        unit $ cmd Shell [Cwd integerGmpBase] "patch -p0 < gmpsrc.patch"
        unit $ cmd Shell [Cwd integerGmpBuild] "patch -p1 < " [integerGmpPatch]
        putBuild $ "| Applied gmpsrc.patch and " ++ takeFileName integerGmpPatch

        -- TODO: What's `chmod +x libraries/integer-gmp/gmp/ln` for?

        -- ./configure
        putBuild "| Running libffi configure..."
        envs <- configureEnvironment
        args <- configureArguments
        unit $ cmd Shell [Cwd integerGmpBuild] "bash configure" envs args

        -- make
        putBuild "| Running make..."
        unit $ cmd Shell "make" ["-C", integerGmpBuild, "MAKEFLAGS="]

        -- copy library and header
        forM_ ["gmp.h", ".libs" -/- "libgmp.a"] $ \file -> do
            let file' = integerGmpBase -/- takeFileName file
            copyFileChanged (integerGmpBuild -/- file) file'
            putBuild $ "| Copy " ++ file ++ " -> " ++ file'

        -- TODO: do we need these as well?
        -- mkdir integerGmpBase -/- objs
        -- unit $ cmd Shell [Cwd integerGmpBase -/- "objs"] "$AR_STAGE1 x ../libgmp.a"
        -- $RANLIB_CMD integerGmpBase -/- "libgmp.a"

        putSuccess "| Successfully build custom library 'integer-gmp'"

    "libraries/integer-gmp/gmp/gmp.h" %> \_ -> need [integerGmpLibrary]

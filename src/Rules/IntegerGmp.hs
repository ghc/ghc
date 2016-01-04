module Rules.IntegerGmp (integerGmpRules, integerGmpLibrary, integerGmpLibraryH) where

import Base
import Expression
import GHC
import Oracles.Config.Setting
import Rules.Actions
import Settings.User

integerGmpBase :: FilePath
integerGmpBase = "libraries" -/- "integer-gmp" -/- "gmp"

integerGmpBuild :: FilePath
integerGmpBuild = integerGmpBase -/- "gmpbuild"

integerGmpLibrary :: FilePath
integerGmpLibrary = integerGmpBase -/- "libgmp.a"

integerGmpLibraryH :: FilePath
integerGmpLibraryH = integerGmpBase -/- "gmp.h"

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
    integerGmpLibrary %> \_ -> do
        when trackBuildSystem $ need [sourcePath -/- "Rules/IntegerGmp.hs"]

        -- remove the old build folder, if it exists.
        liftIO $ removeFiles integerGmpBuild ["//*"]
        liftIO $ removeFiles (integerGmpBase -/- "objs") ["//*"]

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

        runMake integerGmpBuild []

        -- copy library and header
        -- TODO: why copy library, can we move it instead?
        forM_ ["gmp.h", ".libs" -/- "libgmp.a"] $ \file ->
            copyFile (integerGmpBuild -/- file) (integerGmpBase -/- takeFileName file)

        let objsDir = integerGmpBase -/- "objs"
        createDirectory objsDir
        build $ fullTarget target Ar [integerGmpLibrary] [objsDir]

        runBuilder Ranlib [integerGmpLibrary]

        putSuccess "| Successfully built custom library 'integer-gmp'"

    integerGmpLibraryH %> \_ -> need [integerGmpLibrary]

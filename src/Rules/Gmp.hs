module Rules.Gmp (gmpRules) where

import Base
import GHC
import Oracles.Setting
import Settings.Packages.IntegerGmp
import Settings.Path
import Target
import Utilities

gmpBase :: FilePath
gmpBase = pkgPath integerGmp -/- "gmp"

gmpLibraryInTreeH :: FilePath
gmpLibraryInTreeH = gmpBuildPath -/- "include/gmp.h"

gmpLibrary :: FilePath
gmpLibrary = gmpBuildPath -/- ".libs/libgmp.a"

gmpMakefile :: FilePath
gmpMakefile = gmpBuildPath -/- "Makefile"

configureEnvironment :: Action [CmdOption]
configureEnvironment = sequence [ builderEnvironment "CC" $ Cc CompileC Stage1
                                , builderEnvironment "AR" (Ar Stage1)
                                , builderEnvironment "NM" Nm ]

gmpRules :: Rules ()
gmpRules = do
    -- Copy appropriate GMP header and object files
    gmpLibraryH %> \header -> do
        windows  <- windowsHost
        configMk <- readFile' $ gmpBase -/- "config.mk"
        if not windows && -- TODO: We don't use system GMP on Windows. Fix?
           any (`isInfixOf` configMk) [ "HaveFrameworkGMP = YES", "HaveLibGmp = YES" ]
        then do
            putBuild "| GMP library/framework detected and will be used"
            copyFile (gmpBase -/- "ghc-gmp.h") header
        else do
            putBuild "| No GMP library/framework detected; in tree GMP will be built"
            need [gmpLibrary]
            createDirectory gmpObjects
            build $ target gmpContext (Ar Stage1) [gmpLibrary] [gmpObjects]
            copyFile (gmpBuildPath -/- "gmp.h") header
            copyFile (gmpBuildPath -/- "gmp.h") gmpLibraryInTreeH

    -- Build in-tree GMP library
    gmpLibrary %> \lib -> do
        build $ target gmpContext (Make gmpBuildPath) [gmpMakefile] [lib]
        putSuccess "| Successfully built custom library 'gmp'"

    -- In-tree GMP header is built in the gmpLibraryH rule
    gmpLibraryInTreeH %> \_ -> need [gmpLibraryH]

    -- This causes integerGmp package to be configured, hence creating the files
    [gmpBase -/- "config.mk", gmpBuildInfoPath] &%> \_ ->
        need [pkgDataFile gmpContext]

    -- Run GMP's configure script
    gmpMakefile %> \mk -> do
        env <- configureEnvironment
        need [mk <.> "in"]
        buildWithCmdOptions env $
            target gmpContext (Configure gmpBuildPath) [mk <.> "in"] [mk]

    -- Extract in-tree GMP sources and apply patches
    gmpMakefile <.> "in" %> \_ -> do
        removeDirectory gmpBuildPath
        -- Note: We use a tarball like gmp-4.2.4-nodoc.tar.bz2, which is
        -- gmp-4.2.4.tar.bz2 repacked without the doc/ directory contents.
        -- That's because the doc/ directory contents are under the GFDL,
        -- which causes problems for Debian.
        tarball <- unifyPath . fromSingleton "Exactly one GMP tarball is expected"
               <$> getDirectoryFiles "" [gmpBase -/- "gmp-tarballs/gmp*.tar.bz2"]

        withTempDir $ \dir -> do
            let tmp = unifyPath dir
            need [tarball]
            build $ target gmpContext Tar [tarball] [tmp]

            let patch     = gmpBase -/- "gmpsrc.patch"
                patchName = takeFileName patch
            copyFile patch $ tmp -/- patchName
            applyPatch tmp patchName

            let name    = dropExtension . dropExtension $ takeFileName tarball
                unpack  = fromMaybe . error $ "gmpRules: expected suffix "
                    ++ "-nodoc (found: " ++ name ++ ")."
                libName = unpack $ stripSuffix "-nodoc" name

            moveDirectory (tmp -/- libName) gmpBuildPath

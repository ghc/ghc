module Rules.Gmp (
    gmpRules, gmpBuildPath, gmpObjectsDir, gmpLibraryH
    ) where

import Base
import Context
import GHC
import Oracles.Setting
import Target
import Utilities

gmpBase :: FilePath
gmpBase = pkgPath integerGmp -/- "gmp"

gmpLibraryInTreeH :: FilePath
gmpLibraryInTreeH = "include/gmp.h"

gmpLibrary :: FilePath
gmpLibrary = ".libs/libgmp.a"

-- | GMP is considered a Stage1 package. This determines GMP build directory.
gmpContext :: Context
gmpContext = vanillaContext Stage1 integerGmp

-- | Build directory for in-tree GMP library.
gmpBuildPath :: Action FilePath
gmpBuildPath = buildRoot <&> (-/- buildDir gmpContext -/- "gmp")

-- | GMP library header, relative to 'gmpBuildPath'.
gmpLibraryH :: FilePath
gmpLibraryH = "include/ghc-gmp.h"

-- | Directory for GMP library object files, relative to 'gmpBuildPath'.
gmpObjectsDir :: FilePath
gmpObjectsDir = "objs"

configureEnvironment :: Action [CmdOption]
configureEnvironment = sequence [ builderEnvironment "CC" $ Cc CompileC Stage1
                                , builderEnvironment "AR" (Ar Unpack Stage1)
                                , builderEnvironment "NM" Nm ]

gmpRules :: Rules ()
gmpRules = do
    -- Copy appropriate GMP header and object files
    root <- buildRootRules
    root <//> gmpLibraryH %> \header -> do
        windows  <- windowsHost
        configMk <- readFile' =<< (gmpBuildPath <&> (-/- "config.mk"))
        if not windows && -- TODO: We don't use system GMP on Windows. Fix?
           any (`isInfixOf` configMk) [ "HaveFrameworkGMP = YES", "HaveLibGmp = YES" ]
        then do
            putBuild "| GMP library/framework detected and will be used"
            copyFile (gmpBase -/- "ghc-gmp.h") header
        else do
            putBuild "| No GMP library/framework detected; in tree GMP will be built"
            gmpPath <- gmpBuildPath
            need [gmpPath -/- gmpLibrary]
            createDirectory (gmpPath -/- gmpObjectsDir)
            top <- topDirectory
            build $ target gmpContext (Ar Unpack Stage1)
                [top -/- gmpPath -/- gmpLibrary] [gmpPath -/- gmpObjectsDir]
            copyFile (gmpPath -/- "gmp.h") header
            copyFile (gmpPath -/- "gmp.h") (gmpPath -/- gmpLibraryInTreeH)

    -- Build in-tree GMP library
    root <//> gmpLibrary %> \lib -> do
        gmpPath <- gmpBuildPath
        build $ target gmpContext (Make gmpPath) [gmpPath -/- "Makefile"] [lib]
        putSuccess "| Successfully built custom library 'gmp'"

    -- In-tree GMP header is built by the gmpLibraryH rule
    root <//> gmpLibraryInTreeH %> \_ -> do
        gmpPath <- gmpBuildPath
        need [gmpPath -/- gmpLibraryH]

    -- This causes integerGmp package to be configured, hence creating the files
    root <//> "gmp/config.mk" %> \_ -> do
        -- setup-config, triggers `ghc-cabal configure`
        -- everything of a package should depend on that
        -- in the first place.
        setupConfig <- contextPath gmpContext <&> (-/- "setup-config")
        need [setupConfig]

    -- Run GMP's configure script
    -- TODO: Get rid of hard-coded @gmp@.
    root <//> "gmp/Makefile" %> \mk -> do
        env     <- configureEnvironment
        gmpPath <- gmpBuildPath
        need [mk <.> "in"]
        buildWithCmdOptions env $
            target gmpContext (Configure gmpPath) [mk <.> "in"] [mk]

    -- Extract in-tree GMP sources and apply patches
    root <//> "gmp/Makefile.in" %> \_ -> do
        gmpPath <- gmpBuildPath
        removeDirectory gmpPath
        -- Note: We use a tarball like gmp-4.2.4-nodoc.tar.bz2, which is
        -- gmp-4.2.4.tar.bz2 repacked without the doc/ directory contents.
        -- That's because the doc/ directory contents are under the GFDL,
        -- which causes problems for Debian.
        tarball <- unifyPath . fromSingleton "Exactly one GMP tarball is expected"
               <$> getDirectoryFiles "" [gmpBase -/- "gmp-tarballs/gmp*.tar.bz2"]

        withTempDir $ \dir -> do
            let tmp = unifyPath dir
            need [tarball]
            build $ target gmpContext (Tar Extract) [tarball] [tmp]

            let patch     = gmpBase -/- "gmpsrc.patch"
                patchName = takeFileName patch
            copyFile patch $ tmp -/- patchName
            applyPatch tmp patchName

            let name    = dropExtension . dropExtension $ takeFileName tarball
                unpack  = fromMaybe . error $ "gmpRules: expected suffix "
                    ++ "-nodoc (found: " ++ name ++ ")."
                libName = unpack $ stripSuffix "-nodoc" name

            moveDirectory (tmp -/- libName) gmpPath

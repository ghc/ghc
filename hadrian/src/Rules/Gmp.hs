module Rules.Gmp (
    gmpRules, gmpBuildPath, gmpObjects, gmpLibraryH
    ) where

import Base
import Context
import Oracles.Setting
import Packages
import Target
import Utilities

-- | Build GMP library objects and return their paths.
gmpObjects :: Action [FilePath]
gmpObjects = do
    gmpPath <- gmpBuildPath
    need [gmpPath -/- gmpLibraryH]
    -- The line below causes a Shake Lint failure on Windows, which forced us to
    -- disable Lint by default. See more details here:
    -- https://ghc.haskell.org/trac/ghc/ticket/15971.
    map unifyPath <$>
        liftIO (getDirectoryFilesIO "" [gmpPath -/- gmpObjectsDir -/- "*.o"])

gmpBase :: FilePath
gmpBase = pkgPath integerGmp -/- "gmp"

gmpLibraryInTreeH :: FilePath
gmpLibraryInTreeH = "include/gmp.h"

gmpLibrary :: FilePath
gmpLibrary = ".libs/libgmp.a"

-- | GMP is considered a Stage1 package. This determines GMP build directory.
gmpContext :: Context
gmpContext = vanillaContext Stage1 integerGmp

-- TODO: Location of 'gmpBuildPath' is important: it should be outside any
-- package build directory, as otherwise GMP's object files will match build
-- patterns of 'compilePackage' rules. We could make 'compilePackage' rules
-- more precise to avoid such spurious matching.
-- | Build directory for in-tree GMP library.
gmpBuildPath :: Action FilePath
gmpBuildPath = buildRoot <&> (-/- stageString (stage gmpContext) -/- "gmp")

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
        configMk <- readFile' =<< (buildPath gmpContext <&> (-/- "config.mk"))
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

    -- Build in-tree GMP library, prioritised so that it matches "before"
    -- the generic .a library rule in Rules.Library, whenever applicable.
    priority 2.0 $ root <//> gmpLibrary %> \lib -> do
        gmpPath <- gmpBuildPath
        build $ target gmpContext (Make gmpPath) [gmpPath -/- "Makefile"] [lib]
        putSuccess "| Successfully built custom library 'gmp'"

    -- In-tree GMP header is built by the gmpLibraryH rule
    root <//> gmpLibraryInTreeH %> \_ -> do
        gmpPath <- gmpBuildPath
        need [gmpPath -/- gmpLibraryH]

    -- This causes integerGmp package to be configured, hence creating the files
    root <//> "gmp/config.mk" %> \_ -> do
        -- Calling 'need' on @setup-config@ triggers 'configurePackage'.
        -- TODO: Shall we run 'configurePackage' directly? Why this indirection?
        setupConfig <- pkgSetupConfigFile gmpContext
        need [setupConfig]

    -- TODO: Get rid of hard-coded @gmp@.
    -- Run GMP's configure script
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

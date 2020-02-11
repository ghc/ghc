module Rules.Gmp (gmpRules, gmpBuildPath, gmpObjects, gmpLibraryH) where

import Base
import Context
import Oracles.Setting
import Packages
import Target
import Utilities
import Hadrian.BuildPath

-- | Build GMP library objects and return their paths.
gmpObjects :: Context -> Action [FilePath]
gmpObjects ctx = do
    gmpBuildP <- gmpBuildPath ctx
    need [gmpBuildP -/- gmpLibraryH]
    -- The line below causes a Shake Lint failure on Windows, which forced us to
    -- disable Lint by default. See more details here:
    -- https://gitlab.haskell.org/ghc/ghc/issues/15971.
    map (unifyPath . (gmpBuildP -/-)) <$>
        liftIO (getDirectoryFilesIO gmpBuildP [gmpObjectsDir -/- "*.o"])

-- TODO: Location of 'gmpBuildPath' is important: it should be outside any
-- package build directory, as otherwise GMP's object files will match build
-- patterns of 'compilePackage' rules. We could make 'compilePackage' rules
-- more precise to avoid such spurious matching.

-- | Build directory for in-tree GMP library.
--
-- E.g. <root>/stageN/gmp/
--
gmpBuildPath :: Context -> Action FilePath
gmpBuildPath ctx = buildRoot <&> (-/- stageString (stage ctx) -/- "gmp")

gmpBase :: FilePath
gmpBase = pkgPath ghcBignum -/- "gmp"

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
    root <- buildRootRules

    -- Read config.mk produced by ghc-bignum's configure
    -- Build in-tree gmp if necessary
    -- Copy header (intree or from our sources) into ghc-bignum/include/ghc-gmp.h
    root -/- "stage*/libraries/ghc-bignum/build/include/ghc-gmp.h" %> \header -> do
        let includeP   = takeDirectory header
            buildP     = takeDirectory includeP
            bignumP    = takeDirectory buildP
            librariesP = takeDirectory bignumP
            stageP     = takeDirectory librariesP
        configMk <- readFile' (buildP -/- "config.mk") -- TODO: we need ghc-bignum to be configured
                                                       -- ensureConfigured ghcBignumCtx
        if not windowsHost && -- TODO: We don't use system GMP on Windows. Fix?
           any (`isInfixOf` configMk) [ "HaveFrameworkGMP = YES", "HaveLibGmp = YES" ]
        then do
            putBuild "| GMP library/framework detected and will be used"
            copyFile (gmpBase -/- "ghc-gmp.h") header
        else do
            putBuild "| No GMP library/framework detected; in tree GMP will be built"
            let intreeHeader = stageP -/- "gmp/ghc.h"
            need [intreeHeader]
            copyFile          intreeHeader header
            -- gmp_wrappers.c needs to find gmp.h so we copy it there too
            copyFileUntracked intreeHeader (includeP -/- "gmp.h")

    -- Build in-tree GMP
    root -/- "stage*/gmp/ghc.h" %> \header -> do
        let includeP = takeDirectory header
            gmpP     = takeDirectory includeP
            stageP   = takeDirectory gmpP
            stageS   = takeFileName stageP
        stage <- parsePath parseStage "<stage>" stageS
        let dummyContext = vanillaContext stage ghcBignum

        need [gmpP -/- "libgmp.a"]
        -- unpack objects from libgmp.a into "objs" directory
        createDirectory (gmpP -/- gmpObjectsDir)
        top <- topDirectory
        build $ target dummyContext (Ar Unpack Stage1)
            [top -/- gmpP -/- "libgmp.a"] [gmpP -/- gmpObjectsDir]
        objs <- liftIO $ getDirectoryFilesIO "." [gmpP -/- gmpObjectsDir -/- "*"]
        produces objs
        -- at this point it should have produced ghc.h too

    -- Build in-tree GMP library, prioritised so that it matches "before"
    -- the generic @.a@ library rule in 'Rules.Library'.
    priority 2.0 $ root -/- "stage*/gmp/libgmp.a" %> \lib -> do
        let gmpP = takeDirectory lib
            stageP = takeDirectory gmpP
            stageS = takeFileName stageP
        stage <- parsePath parseStage "<stage>" stageS
        let dummyContext = vanillaContext stage ghcBignum
        build $ target dummyContext (Make gmpP) [gmpP -/- "Makefile"] [lib]
        copyFileUntracked ".libs/libgmp.a" lib
        putSuccess "| Successfully built custom library 'gmp'"

    -- Run GMP's configure script
    root -/- "stage*/gmp" -/- "Makefile" %> \mk -> do
        let gmpP   = takeDirectory mk
            stageP = takeDirectory gmpP
            stageS = takeFileName stageP
        stage <- parsePath parseStage "<stage>" stageS
        env <- configureEnvironment
        need [mk <.> "in"]
        let dummyContext = vanillaContext stage ghcBignum
        buildWithCmdOptions env $
            target dummyContext (Configure gmpP) [mk <.> "in"] [mk]

    -- Extract in-tree GMP sources and apply patches
    fmap ((root -/- "stage*/gmp") -/-) ["Makefile.in", "configure"] &%> \[mkFileP,_] -> do
        top <- topDirectory
        let gmpP   = takeDirectory mkFileP
            stageP = takeDirectory gmpP
            stageS = takeFileName stageP
        stage <- parsePath parseStage "<stage>" stageS
        removeDirectory gmpP
        -- Note: We use a tarball like gmp-4.2.4-nodoc.tar.bz2, which is
        -- gmp-4.2.4.tar.bz2 repacked without the doc/ directory contents.
        -- That's because the doc/ directory contents are under the GFDL,
        -- which causes problems for Debian.
        tarball <- unifyPath . fromSingleton "Exactly one GMP tarball is expected"
               <$> getDirectoryFiles top [gmpBase -/- "gmp-tarballs/gmp*.tar.bz2"]

        withTempDir $ \dir -> do
            let tmp = unifyPath dir
            need [top -/- tarball]
            let dummyContext = vanillaContext stage ghcBignum
            build $ target dummyContext (Tar Extract) [top -/- tarball] [tmp]

            let patch     = gmpBase -/- "gmpsrc.patch"
                patchName = takeFileName patch
            copyFile patch $ tmp -/- patchName
            applyPatch tmp patchName

            let name    = dropExtension . dropExtension $ takeFileName tarball
                unpack  = fromMaybe . error $ "gmpRules: expected suffix "
                    ++ "-nodoc (found: " ++ name ++ ")."
                libName = unpack $ stripSuffix "-nodoc" name

            moveDirectory (tmp -/- libName) gmpP

{-# LANGUAGE GADTs #-}

module Rules.Gmp (gmpRules, gmpBuildPath, gmpObjects) where

import Base
import Context
import Oracles.Setting
import Oracles.Flag
import Packages
import Target
import Utilities
import Hadrian.BuildPath
import Hadrian.Expression
import Settings.Builders.Common (cArgs, getStagedCCFlags)

-- | Build in-tree GMP library objects (if GmpInTree flag is set) and return
-- their paths.
gmpObjects :: Stage -> Action [FilePath]
gmpObjects s = do
  isInTree <- buildFlag GmpInTree s
  if not isInTree
    then return []
    else do
      -- Indirectly ensure object creation
      let ctx = vanillaContext s ghcBignum
      ghcBignumPath <- buildPath ctx
      need [ghcBignumPath -/- "include/ghc-gmp.h"]

      gmpPath <- gmpIntreePath s
      map (unifyPath . (gmpPath -/-)) <$>
          -- Note we don't track the object files of the in-tree GMP library (cf
          -- #15971).
          liftIO (getDirectoryFilesIO gmpPath [gmpObjectsDir -/- "*.o"])

-- | Build directory for in-tree GMP library
--    <root>/stageN/gmp/gmpbuild
gmpBuildPath :: Stage -> Action FilePath
gmpBuildPath s = gmpIntreePath s <&> (-/- "gmpbuild")

-- | Root directory for in-tree GMP library
--    <root>/stageN/gmp
gmpIntreePath :: Stage -> Action FilePath
gmpIntreePath s = buildRoot <&> (-/- stageString s -/- "gmp")

-- | Directory for in-tree GMP library object files, relative to 'gmpIntreePath'.
gmpObjectsDir :: FilePath
gmpObjectsDir = "objs"

gmpRules :: Rules ()
gmpRules = do
    root <- buildRootRules

    let
      -- Path to libraries/integer-gmp/gmp in the source tree
      gmpBase :: FilePath
      gmpBase = pkgPath ghcBignum -/- "gmp"

    -- Build in-tree gmp if necessary
    -- Produce: ghc-bignum/build/include/ghc-gmp.h
    --   In-tree: copy gmp.h from in-tree build
    --   External: copy ghc-gmp.h from base sources
    root -/- "stage*/libraries/ghc-bignum/build/include/ghc-gmp.h" %> \header -> do
        let includeP   = takeDirectory header
            buildP     = takeDirectory includeP
            packageP   = takeDirectory buildP
            librariesP = takeDirectory packageP
            stageP     = takeDirectory librariesP
        stage <- parsePath parseStage "<stage>" (takeFileName stageP)

        isInTree <- buildFlag GmpInTree stage

        if isInTree
        then do
            putBuild "| In tree GMP will be built"
            let intreeHeader = stageP -/- "gmp/gmp.h"
            need [intreeHeader]
            copyFile intreeHeader header
        else do
            putBuild "| System GMP library/framework will be used"
            copyFile (gmpBase -/- "ghc-gmp.h") header

    -- Build in-tree GMP library for the current stage, prioritised so that it
    -- matches "before" the generic @.a@ library rule in 'Rules.Library'.
    priority 2.0 $ do

        let
          -- parse a path of the form "//stage*/gmp/xxx" and returns a vanilla
          -- context from it for ghc-bignum package.
          makeGmpPathContext gmpP = do
               let
                   stageP   = takeDirectory gmpP
                   stageS   = takeFileName stageP
               stage <- parsePath parseStage "<stage>" stageS
               pure (vanillaContext stage ghcBignum)

          gmpPath = root -/- "stage*/gmp"

        -- Build in-tree gmp. Produce:
        --  - <root>/stageN/gmp/gmp.h
        --  - <root>/stageN/gmp/libgmp.a
        --  - <root>/stageN/gmp/objs/*.o (unpacked objects from libgmp.a)
        (gmpPath -/- "libgmp.a" :& gmpPath -/- "gmp.h" :& Nil) &%>
          \( lib :& header :& _) -> do
            let gmpP = takeDirectory lib
            ctx <- makeGmpPathContext gmpP
            -- build libgmp.a via gmp's Makefile
            build $ target ctx (Make (gmpP -/- "gmpbuild")) [gmpP -/- "gmpbuild/Makefile"] []
            -- copy header and lib to their final destination
            copyFileUntracked (gmpP -/- "gmpbuild/.libs/libgmp.a") lib
            copyFileUntracked (gmpP -/- "gmpbuild/gmp.h")          header
            -- we also unpack objects from libgmp.a into "objs" directory
            createDirectory (gmpP -/- gmpObjectsDir)
            top <- topDirectory
            build $ target ctx (Ar Unpack (stage ctx))
                [top -/- gmpP -/- "libgmp.a"] [gmpP -/- gmpObjectsDir]
            objs <- liftIO $ getDirectoryFilesIO "." [gmpP -/- gmpObjectsDir -/- "*"]
            produces objs
            putSuccess "| Successfully built custom library 'gmp'"

        -- Run GMP's configure script. Produce:
        --  - <root>/stageN/gmp/gmpbuild/Makefile
        gmpPath -/- "gmpbuild/Makefile" %> \mk -> do
            let gmpBuildP = takeDirectory mk
                gmpP      = takeDirectory gmpBuildP
            ctx <- makeGmpPathContext gmpP
            cFlags <- interpretInContext ctx $ mconcat [ cArgs, getStagedCCFlags ]
            env <- sequence
                     [ builderEnvironment "CC" $ Cc CompileC (stage ctx)
                     , return . AddEnv "CFLAGS" $ unwords cFlags
                     , builderEnvironment "AR" (Ar Unpack (stage ctx))
                     , builderEnvironment "NM" (Nm (stage ctx))
                     ]
            need [mk <.> "in"]
            buildWithCmdOptions env $
                target ctx (Configure gmpBuildP) [mk <.> "in"] [mk]

        -- Extract in-tree GMP sources and apply patches. Produce
        --  - <root>/stageN/gmp/gmpbuild/Makefile.in
        --  - <root>/stageN/gmp/gmpbuild/configure
        (gmpPath -/- "gmpbuild/Makefile.in" :& gmpPath -/- "gmpbuild/configure" :& Nil)
           &%> \( mkIn :& _ ) -> do
            top <- topDirectory
            let gmpBuildP = takeDirectory mkIn
                gmpP      = takeDirectory gmpBuildP
            ctx <- makeGmpPathContext gmpP
            removeDirectory gmpBuildP
            -- Note: We use a tarball like gmp-4.2.4-nodoc.tar.xz, which is
            -- gmp-4.2.4.tar.xz repacked without the doc/ directory contents.
            -- That's because the doc/ directory contents are under the GFDL,
            -- which causes problems for Debian.
            tarball <- unifyPath . fromSingleton "Exactly one GMP tarball is expected"
                   <$> getDirectoryFiles top [gmpBase -/- "gmp-tarballs/gmp*.tar.xz"]

            withTempDir $ \dir -> do
                let tmp = unifyPath dir
                need [top -/- tarball]
                build $ target ctx (Tar Extract) [top -/- tarball] [tmp]

                let name    = dropExtension . dropExtension $ takeFileName tarball
                    unpack  = fromMaybe . error $ "gmpRules: expected suffix "
                        ++ "-nodoc (found: " ++ name ++ ")."
                    libName = unpack $ stripSuffix "-nodoc" name

                moveDirectory (tmp -/- libName) gmpBuildP

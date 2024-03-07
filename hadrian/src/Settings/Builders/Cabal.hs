module Settings.Builders.Cabal (cabalBuilderArgs) where

import Development.Shake.FilePath
import Hadrian.Haskell.Cabal

import Builder
import Context
import Flavour
import Packages
import Settings.Builders.Common
import qualified Settings.Builders.Common as S
import Control.Exception (assert)
import qualified Data.Set as Set
import System.Directory
import Settings.Program (programContext)
import GHC.Toolchain (ccLinkProgram, tgtCCompilerLink, targetPlatformTriple)
import GHC.Toolchain.Program (prgFlags)

cabalBuilderArgs :: Args
cabalBuilderArgs = cabalSetupArgs <> cabalInstallArgs

cabalInstallArgs :: Args
cabalInstallArgs = builder (Cabal Install) ?  do
  pkg <- getPackage
  root <- exprIO . makeAbsolute =<< getBuildRoot
  let pgmName
        | pkg == ghc    = "ghc"
        | pkg == hpcBin = "hpc"
        | otherwise     = pkgName pkg
  assertNoBuildRootLeak $
    mconcat [ arg $ "--store-dir="   ++ (root -/- "stage-cabal" -/- "cabal-store")
            , if isProgram pkg then arg "install" else arg "build"
            , if isProgram pkg then (arg $ "exe:" ++ pgmName) else (arg $ pkgName pkg)
            , commonReinstallCabalArgs
            , if isProgram pkg then extraInstallArgs else mempty
            ]

-- | Checks that _build/stageN/lib/* doesn't leak into the arguments for
-- reinstallable GHC. If this assert fails, then GHC probably isn't
-- properly reinstallable.
-- We allow "package.conf.d" however since we do need to read the package environment
-- of the stage 2 compiler
assertNoBuildRootLeak :: Args -> Args
assertNoBuildRootLeak args = do
  libPaths <- expr $ mapM stageLibPath allStages
  xs <- args
  pure $ assert (not $ any (\arg -> or [libPath `isInfixOf` arg && not ("package.conf.d" `isSuffixOf` arg)
                                       | libPath <- libPaths]) xs)
                xs

extraInstallArgs :: Args
extraInstallArgs = do
    root      <- getBuildRoot
    mconcat [ arg $ "--install-method=copy"
            , arg $ "--overwrite-policy=always"
            , arg $ "--installdir="  ++ (root -/- "stage-cabal" -/- "cabal-bin") ]

commonReinstallCabalArgs :: Args
commonReinstallCabalArgs = do
    top       <- expr topDirectory
    root      <- getBuildRoot
    compiler  <- expr $ programPath =<< programContext Stage1 ghc
    mconcat [ arg "--project-file"
            , arg $ top -/- "cabal.project-reinstall"
            , arg "--distdir"
            , arg $ root -/- "stage-cabal" -/- "dist-newstyle"
            , arg $ "--with-compiler=" ++ top -/- compiler
            , arg "--enable-executable-dynamic"
            , arg "--enable-library-vanilla"
            ]

cabalSetupArgs :: Args
cabalSetupArgs = builder (Cabal Setup) ? do
  top   <- expr topDirectory
  stage <- getStage
  path  <- getContextPath
  mconcat [ arg "configure"
          , arg "--distdir"
          , arg $ top -/- path
          , commonCabalArgs stage
          , configureStageArgs
          ]

commonCabalArgs :: Stage -> Args
commonCabalArgs stage = do
  pkg       <- getPackage
  package_id <- expr $ pkgUnitId stage pkg
  let prefix = "${pkgroot}" ++ (if windowsHost then "" else "/..")
  mconcat [ -- Don't strip libraries when cross compiling.
            -- TODO: We need to set @--with-strip=(stripCmdPath :: Action FilePath)@,
            -- and if it's @:@ disable stripping as well. As it is now, I believe
            -- we might have issues with stripping on Windows, as I can't see a
            -- consumer of 'stripCmdPath'.
            -- TODO: See https://github.com/snowleopard/hadrian/issues/549.
            -- TODO: MP should check per-stage rather than a global CrossCompiling, but not going to cause bugs
              flag CrossCompiling ? pure [ "--disable-executable-stripping"
                                         , "--disable-library-stripping" ]
            -- We don't want to strip the debug RTS
            , S.package rts ? pure [ "--disable-executable-stripping"
                                  , "--disable-library-stripping" ]
            , arg "--cabal-file"
            , arg $ pkgCabalFile pkg
            , arg "--ipid"
            , arg package_id
            , arg "--prefix"
            , arg prefix

            -- NB: this is valid only because Hadrian puts the @doc@ and
            -- @libraries@ folders in the same relative position:
            --
            --   * libraries in @_build/stageN/libraries@
            --   * docs in @_build/doc/html/libraries@
            --
            -- This doesn't hold if we move the @doc@ folder anywhere else.
            , arg "--htmldir"
            , arg $ "${pkgroot}/../../doc/html/libraries/" ++ package_id

            -- These trigger a need on each dependency, so every important to need
            -- them in parallel or  it linearises the build of Ghc and GhcPkg
            , withStageds [Ghc CompileHs, GhcPkg Update, Cc CompileC, Ar Pack]
            , withBuilderArgs (Ghc CompileHs stage)
            , withBuilderArgs (GhcPkg Update stage)
            , bootPackageDatabaseArgs
            , libraryArgs
            , bootPackageConstraints
            , notStage0 ? with (Ld stage)
            , with Alex
            , with Happy
            -- Update Target.trackArgument if changing these:
            ]

-- TODO: Isn't vanilla always built? If yes, some conditions are redundant.
-- TODO: Need compiler_stage1_CONFIGURE_OPTS += --disable-library-for-ghci?
-- TODO: should `elem` be `wayUnit`?
-- This approach still doesn't work. Previously libraries were build only in the
-- Default flavours and not using context.
libraryArgs :: Args
libraryArgs = do
    flavourWays <- getLibraryWays
    contextWay  <- getWay
    package     <- getPackage
    stage       <- getStage
    withGhci    <- expr (ghcWithInterpreter stage)
    dynPrograms <- expr (flavour >>= flip dynamicGhcPrograms stage)
    ghciObjsSupported <- expr (targetSupportsGhciObjects stage)
    let ways = Set.insert contextWay flavourWays
        hasVanilla = vanilla `elem` ways
        hasProfiling = any (wayUnit Profiling) ways
        hasProfilingShared = profilingDynamic `elem` ways
        hasDynamic = any (wayUnit Dynamic) ways
    pure [ if hasVanilla
           then  "--enable-library-vanilla"
           else "--disable-library-vanilla"
         , if hasProfiling
           then  "--enable-library-profiling"
           else "--disable-library-profiling"
         , if hasProfilingShared
            then "--enable-profiling-shared"
            else "--disable-profiling-shared"
         , if ghciObjsSupported &&
              (hasVanilla || hasProfiling) &&
              package /= rts && withGhci && not dynPrograms
           then  "--enable-library-for-ghci"
           else "--disable-library-for-ghci"
         , if hasDynamic
           then  "--enable-shared"
           else "--disable-shared" ]

-- | Configure args with stage/lib specific include directories and settings
configureStageArgs :: Args
configureStageArgs = do
  let cFlags  = getStagedCCFlags
      linkFlags = prgFlags . ccLinkProgram . tgtCCompilerLink <$> getStagedTarget
  mconcat [ configureArgs cFlags linkFlags
          , ghcVersionH
          ]

ghcVersionH :: Args
ghcVersionH = notStage0 ? do
    let h = "rts/include/ghcversion.h"
    expr $ need [h]
    arg $ "--ghc-option=-ghcversion-file=" <> h

configureArgs :: Args -> Args -> Args
configureArgs cFlags' ldFlags' = do

    top  <- expr topDirectory
    pkg  <- getPackage
    let conf key expr = do
            values <- unwords <$> expr
            not (null values) ?
                arg ("--configure-option=" ++ key ++ "=" ++ values)
        cFlags   = mconcat [ remove ["-Werror"] cArgs
                           , getStagedCCFlags
                           -- See https://github.com/snowleopard/hadrian/issues/523
                           , arg $ "-iquote"

                           , arg $ top -/- pkgPath pkg
                           , cFlags'
                           ]
        ldFlags  = ldArgs <> ldFlags'
    let predStage' s = case s of {Stage0 {} -> stage0InTree ; _ -> predStage s }
    mconcat
        [ conf "CFLAGS"   cFlags
        , conf "LDFLAGS"  ldFlags
        , conf "--with-iconv-includes"    $ arg =<< staged (buildSetting IconvIncludeDir)
        , conf "--with-iconv-libraries"   $ arg =<< staged (buildSetting IconvLibDir)
        , conf "--with-gmp-includes"      $ arg =<< staged (buildSetting GmpIncludeDir)
        , conf "--with-gmp-libraries"     $ arg =<< staged (buildSetting GmpLibDir)
        , conf "--with-curses-libraries"  $ arg =<< staged (buildSetting CursesLibDir)
        , conf "--host"                   $ arg =<< flip queryTarget targetPlatformTriple  . predStage' =<< getStage
        , conf "--with-cc" $ arg =<< getBuilderPath . (Cc CompileC) =<< getStage
        , ghcVersionH
        ]

bootPackageConstraints :: Args
bootPackageConstraints = (stage0InTree ==) <$> getStage ? do
    bootPkgs <- expr $ stagePackages stage0InTree
    let pkgs = filter (\p -> p /= compiler && isLibrary p) bootPkgs
    constraints <- expr $ forM (sort pkgs) $ \pkg -> do
        version <- pkgVersion pkg
        return $ ((pkgName pkg ++ " == ") ++) version
    pure $ concat [ ["--constraint", c] | c <- constraints ]

withBuilderKey :: Builder -> String
withBuilderKey b = case b of
    Ar _ _     -> "--with-ar="
    Ld _       -> "--with-ld="
    Cc  _ _    -> "--with-gcc="
    Ghc _ _    -> "--with-ghc="
    Alex       -> "--with-alex="
    Happy      -> "--with-happy="
    GhcPkg _ _ -> "--with-ghc-pkg="
    _          -> error $ "withBuilderKey: not supported builder " ++ show b

-- | Add arguments to builders if needed.
withBuilderArgs :: Builder -> Args
withBuilderArgs b = case b of
    Ghc _ stage -> do
      top   <- expr topDirectory
      pkgDb <- expr $ packageDbPath (PackageDbLoc stage Inplace)
      -- GHC starts with a nonempty package DB stack, so we need to tell it
      -- to empty the stack first for it to truly consider only the package
      -- DB we explicitly provide. See #17468.
      notStage0 ? arg ("--ghc-option=-no-global-package-db") <>
                  arg ("--ghc-option=-package-db=" ++ top -/- pkgDb)
    GhcPkg _ stage -> do
      top   <- expr topDirectory
      pkgDb <- expr $ packageDbPath (PackageDbLoc stage Inplace)
      notStage0 ? arg ("--ghc-pkg-option=--global-package-db=" ++ top -/- pkgDb)
    _          -> return [] -- no arguments

-- | Expression 'with Alex' appends "--with-alex=/path/to/alex" and needs Alex.
with :: Builder -> Args
with b = withs [b]

-- | Expression 'with Alex' appends "--with-alex=/path/to/alex" and needs Alex.
withs :: [Builder] -> Args
withs bs = do
    paths <- filter (not . null . snd) <$> mapM (\b -> (b,) <$> getBuilderPath b) bs
    let bs = map fst paths
    expr $ (needBuilders bs)
    top <- expr topDirectory
    mconcat $ map (\(b, path) ->
        -- Do not inject top, if we have a bare name. E.g. do not turn
        -- `ar` into `$top/ar`. But let `ar` be `ar` as found on $PATH.
        arg  $ withBuilderKey b ++ unifyPath (if path /= takeFileName path
                                              then top </> path
                                              else path)) paths

withStageds :: [Stage -> Builder] -> Args
withStageds sb = do
  st <- getStage
  withs (map (\f -> f st) sb)

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
import System.Directory
import Settings.Program (programContext)

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
            , arg "install"
            , if isProgram pkg then arg $ "exe:" ++ pgmName else mconcat [arg "--lib", arg $ pkgName pkg]
            , commonReinstallCabalArgs
            ]

-- | Checks that _build/stageN/lib/* doesn't leak into the arguments for
-- reinstallable GHC. If this assert fails, then GHC probably isn't
-- properly reinstallable.
-- We allow "package.conf.d" however since we do need to read the package environment
-- of the stage 2 compiler
assertNoBuildRootLeak :: Args -> Args
assertNoBuildRootLeak args = do
  libPaths <- expr $ mapM stageLibPath [Stage0 ..]
  xs <- args
  pure $ assert (not $ any (\arg -> or [libPath `isInfixOf` arg && not ("package.conf.d" `isSuffixOf` arg)
                                       | libPath <- libPaths]) xs)
                xs

commonReinstallCabalArgs :: Args
commonReinstallCabalArgs = do
    top       <- expr topDirectory
    root      <- getBuildRoot
    threads   <- shakeThreads <$> expr getShakeOptions
    _pkg      <- getPackage
    compiler  <- expr $ programPath =<< programContext Stage1 ghc
    mconcat [ arg "--project-file"
            , arg $ top -/- "cabal.project-reinstall"
            , arg "--distdir"
            , arg $ root -/- "stage-cabal" -/- "dist-newstyle"
            , arg ("--ghc-option=-j" ++ show threads)
            , arg $ "--install-method=copy"
            , arg $ "--overwrite-policy=always"
            , arg $ "--with-compiler=" ++ top -/- compiler
            , arg $ "--installdir="  ++ (root -/- "stage-cabal" -/- "cabal-bin")
            , arg $ "--package-env=" ++ (root -/- "stage-cabal" -/- "cabal-packages")
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
  verbosity <- expr getVerbosity
  pkg       <- getPackage
  let prefix = "${pkgroot}" ++ (if windowsHost then "" else "/..")
  mconcat [ -- Don't strip libraries when cross compiling.
            -- TODO: We need to set @--with-strip=(stripCmdPath :: Action FilePath)@,
            -- and if it's @:@ disable stripping as well. As it is now, I believe
            -- we might have issues with stripping on Windows, as I can't see a
            -- consumer of 'stripCmdPath'.
            -- TODO: See https://github.com/snowleopard/hadrian/issues/549.
              flag CrossCompiling ? pure [ "--disable-executable-stripping"
                                         , "--disable-library-stripping" ]
            -- We don't want to strip the debug RTS
            , S.package rts ? pure [ "--disable-executable-stripping"
                                  , "--disable-library-stripping" ]
            , arg "--cabal-file"
            , arg $ pkgCabalFile pkg
            , arg "--ipid"
            , arg "$pkg-$version"
            , arg "--prefix"
            , arg prefix

            -- NB: this is valid only because Hadrian puts the @docs@ and
            -- @libraries@ folders in the same relative position:
            --
            --   * libraries in @_build/stageN/libraries@
            --   * docs in @_build/docs/html/libraries@
            --
            -- This doesn't hold if we move the @docs@ folder anywhere else.
            , arg "--htmldir"
            , arg $ "${pkgroot}/../../docs/html/libraries/" ++ pkgName pkg

            , withStaged $ Ghc CompileHs
            , withBuilderArgs (Ghc CompileHs stage)
            , withStaged (GhcPkg Update)
            , withBuilderArgs (GhcPkg Update stage)
            , bootPackageDatabaseArgs
            , libraryArgs
            , bootPackageConstraints
            , withStaged $ Cc CompileC
            , notStage0 ? with (Ld stage)
            , withStaged (Ar Pack)
            , with Alex
            , with Happy
            -- Update Target.trackArgument if changing these:
            , verbosity < Verbose ?
              pure [ "-v0", "--configure-option=--quiet"
                   , "--configure-option=--disable-option-checking" ] ]

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
    withGhci    <- expr ghcWithInterpreter
    dynPrograms <- expr (flavour >>= dynamicGhcPrograms)
    let ways = flavourWays ++ [contextWay]
        hasVanilla = vanilla `elem` ways
        hasProfiling = any (wayUnit Profiling) ways
        hasDynamic = any (wayUnit Dynamic) ways
    pure [ if hasVanilla
           then  "--enable-library-vanilla"
           else "--disable-library-vanilla"
         , if hasProfiling
           then  "--enable-library-profiling"
           else "--disable-library-profiling"
         , if (hasVanilla || hasProfiling) &&
              package /= rts && withGhci && not dynPrograms
           then  "--enable-library-for-ghci"
           else "--disable-library-for-ghci"
         , if hasDynamic
           then  "--enable-shared"
           else "--disable-shared" ]

-- | Configure args with stage/lib specific include directories and settings
configureStageArgs :: Args
configureStageArgs = do
  let cFlags  = getStagedSettingList ConfCcArgs
      ldFlags = getStagedSettingList ConfGccLinkerArgs
  mconcat [ configureArgs cFlags ldFlags
          , notStage0 ? arg "--ghc-option=-ghcversion-file=rts/include/ghcversion.h"
          ]


-- TODO: LD_OPTS?
configureArgs :: Args -> Args -> Args
configureArgs cFlags' ldFlags' = do

    top  <- expr topDirectory
    pkg  <- getPackage
    let conf key expr = do
            values <- unwords <$> expr
            not (null values) ?
                arg ("--configure-option=" ++ key ++ "=" ++ values)
        cFlags   = mconcat [ remove ["-Werror"] cArgs
                           , getStagedSettingList ConfCcArgs
                           -- See https://github.com/snowleopard/hadrian/issues/523
                           , arg $ "-iquote"

                           , arg $ top -/- pkgPath pkg
                           , cFlags'
                           ]
        ldFlags  = ldArgs <> ldFlags'
    cldFlags <- unwords <$> (cFlags <> ldFlags)
    mconcat
        [ conf "CFLAGS"   cFlags
        , conf "LDFLAGS"  ldFlags
        , not (null cldFlags) ? arg ("--gcc-options=" ++ cldFlags)
        , conf "--with-iconv-includes"    $ arg =<< getSetting IconvIncludeDir
        , conf "--with-iconv-libraries"   $ arg =<< getSetting IconvLibDir
        , conf "--with-gmp-includes"      $ arg =<< getSetting GmpIncludeDir
        , conf "--with-gmp-libraries"     $ arg =<< getSetting GmpLibDir
        , conf "--with-curses-libraries"  $ arg =<< getSetting CursesLibDir
        , conf "--host"                   $ arg =<< getSetting TargetPlatformFull
        , conf "--with-cc" $ arg =<< getBuilderPath . (Cc CompileC) =<< getStage
        , notStage0 ? arg "--ghc-option=-ghcversion-file=rts/include/ghcversion.h"
        ]

bootPackageConstraints :: Args
bootPackageConstraints = stage0 ? do
    bootPkgs <- expr $ stagePackages Stage0
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
      pkgDb <- expr $ packageDbPath stage
      -- GHC starts with a nonempty package DB stack, so we need to tell it
      -- to empty the stack first for it to truly consider only the package
      -- DB we explicitly provide. See #17468.
      notStage0 ? arg ("--ghc-option=-no-global-package-db") <>
                  arg ("--ghc-option=-package-db=" ++ top -/- pkgDb)
    GhcPkg _ stage -> do
      top   <- expr topDirectory
      pkgDb <- expr $ packageDbPath stage
      notStage0 ? arg ("--ghc-pkg-option=--global-package-db=" ++ top -/- pkgDb)
    _          -> return [] -- no arguments

-- | Expression 'with Alex' appends "--with-alex=/path/to/alex" and needs Alex.
with :: Builder -> Args
with b = do
    path <- getBuilderPath b
    if null path then mempty else do
        top <- expr topDirectory
        expr $ needBuilder b
        -- Do not inject top, if we have a bare name. E.g. do not turn
        -- `ar` into `$top/ar`. But let `ar` be `ar` as found on $PATH.
        arg  $ withBuilderKey b ++ unifyPath (if path /= takeFileName path
                                              then top </> path
                                              else path)

withStaged :: (Stage -> Builder) -> Args
withStaged sb = with . sb =<< getStage

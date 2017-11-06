module Settings.Builders.GhcCabal (
    ghcCabalBuilderArgs
    ) where

import Hadrian.Haskell.Cabal

import Context
import Flavour
import Settings.Builders.Common

ghcCabalBuilderArgs :: Args
ghcCabalBuilderArgs = builder GhcCabal ? do
    verbosity <- expr getVerbosity
    top       <- expr topDirectory
    path      <- getBuildPath
    notStage0 ? expr (need inplaceLibCopyTargets)
    mconcat [ arg "configure"
            , arg =<< pkgPath <$> getPackage
            , arg $ top -/- path
            , withStaged $ Ghc CompileHs
            , withStaged (GhcPkg Update)
            , bootPackageDatabaseArgs
            , libraryArgs
            , configureArgs
            , bootPackageConstraints
            , withStaged $ Cc CompileC
            , notStage0 ? with Ld
            , withStaged (Ar Pack)
            , with Alex
            , with Happy
            , verbosity < Chatty ? pure [ "-v0", "--configure-option=--quiet"
                , "--configure-option=--disable-option-checking"  ] ]

-- TODO: Isn't vanilla always built? If yes, some conditions are redundant.
-- TODO: Need compiler_stage1_CONFIGURE_OPTS += --disable-library-for-ghci?
libraryArgs :: Args
libraryArgs = do
    ways        <- getLibraryWays
    withGhci    <- expr ghcWithInterpreter
    dynPrograms <- dynamicGhcPrograms <$> expr flavour
    pure [ if vanilla `elem` ways
           then  "--enable-library-vanilla"
           else "--disable-library-vanilla"
         , if vanilla `elem` ways && withGhci && not dynPrograms
           then  "--enable-library-for-ghci"
           else "--disable-library-for-ghci"
         , if profiling `elem` ways
           then  "--enable-library-profiling"
           else "--disable-library-profiling"
         , if dynamic `elem` ways
           then  "--enable-shared"
           else "--disable-shared" ]

-- TODO: LD_OPTS?
configureArgs :: Args
configureArgs = do
    top  <- expr topDirectory
    root <- getBuildRoot
    let conf key expr = do
            values <- unwords <$> expr
            not (null values) ?
                arg ("--configure-option=" ++ key ++ "=" ++ values)
        cFlags   = mconcat [ remove ["-Werror"] cArgs
                           , getStagedSettingList ConfCcArgs
                           , arg $ "-I" ++ top -/- root -/- generatedDir ]
        ldFlags  = ldArgs  <> (getStagedSettingList ConfGccLinkerArgs)
        cppFlags = cppArgs <> (getStagedSettingList ConfCppArgs)
    cldFlags <- unwords <$> (cFlags <> ldFlags)
    mconcat
        [ conf "CFLAGS"   cFlags
        , conf "LDFLAGS"  ldFlags
        , conf "CPPFLAGS" cppFlags
        , not (null cldFlags) ? arg ("--gcc-options=" ++ cldFlags)
        , conf "--with-iconv-includes"    $ arg =<< getSetting IconvIncludeDir
        , conf "--with-iconv-libraries"   $ arg =<< getSetting IconvLibDir
        , conf "--with-gmp-includes"      $ arg =<< getSetting GmpIncludeDir
        , conf "--with-gmp-libraries"     $ arg =<< getSetting GmpLibDir
        , conf "--with-curses-libraries"  $ arg =<< getSetting CursesLibDir
        , crossCompiling ? (conf "--host" $ arg =<< getSetting TargetPlatformFull)
        , conf "--with-cc" $ arg =<< getBuilderPath . (Cc CompileC) =<< getStage ]

bootPackageConstraints :: Args
bootPackageConstraints = stage0 ? do
    bootPkgs <- expr $ stagePackages Stage0
    let pkgs = filter (\p -> p /= compiler && isLibrary p) bootPkgs
    constraints <- expr $ fmap catMaybes $ forM (sort pkgs) $ \pkg -> do
        version <- traverse pkgVersion (pkgCabalFile pkg)
        return $ fmap ((pkgName pkg ++ " == ") ++) version
    pure $ concat [ ["--constraint", c] | c <- constraints ]

cppArgs :: Args
cppArgs = do
    root <- getBuildRoot
    arg $ "-I" ++ root -/- generatedDir

withBuilderKey :: Builder -> String
withBuilderKey b = case b of
    Ar _ _     -> "--with-ar="
    Ld         -> "--with-ld="
    Cc  _ _    -> "--with-gcc="
    Ghc _ _    -> "--with-ghc="
    Alex       -> "--with-alex="
    Happy      -> "--with-happy="
    GhcPkg _ _ -> "--with-ghc-pkg="
    _          -> error $ "withBuilderKey: not supported builder " ++ show b

-- Expression 'with Alex' appends "--with-alex=/path/to/alex" and needs Alex.
with :: Builder -> Args
with b = do
    path <- getBuilderPath b
    if (null path) then mempty else do
        top  <- expr topDirectory
        expr $ needBuilder b
        arg $ withBuilderKey b ++ unifyPath (top </> path)

withStaged :: (Stage -> Builder) -> Args
withStaged sb = with . sb =<< getStage


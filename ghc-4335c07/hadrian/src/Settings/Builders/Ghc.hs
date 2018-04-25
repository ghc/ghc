module Settings.Builders.Ghc (ghcBuilderArgs, haddockGhcArgs) where

import Hadrian.Haskell.Cabal

import Flavour
import Rules.Gmp
import Settings.Builders.Common
import Settings.Warnings

ghcBuilderArgs :: Args
ghcBuilderArgs = mconcat [compileAndLinkHs, compileC, findHsDependencies]

compileAndLinkHs :: Args
compileAndLinkHs = (builder (Ghc CompileHs) ||^ builder (Ghc LinkHs)) ? do
    needTouchy
    mconcat [ arg "-Wall"
            , commonGhcArgs
            , splitObjectsArgs
            , ghcLinkArgs
            , defaultGhcWarningsArgs
            , builder (Ghc CompileHs) ? arg "-c"
            , getInputs
            , arg "-o", arg =<< getOutput ]

needTouchy :: Expr ()
needTouchy = notStage0 ? windowsHost ? do
    touchyPath <- expr $ programPath (vanillaContext Stage0 touchy)
    expr $ need [touchyPath]

compileC :: Args
compileC = builder (Ghc CompileCWithGhc) ? do
    way <- getWay
    let ccArgs = [ getPkgDataList CcArgs
                 , getStagedSettingList ConfCcArgs
                 , cIncludeArgs
                 , Dynamic `wayUnit` way ? pure [ "-fPIC", "-DDYNAMIC" ] ]
    mconcat [ arg "-Wall"
            , ghcLinkArgs
            , commonGhcArgs
            , mconcat (map (map ("-optc" ++) <$>) ccArgs)
            , defaultGhcWarningsArgs
            , arg "-c"
            , getInputs
            , arg "-o"
            , arg =<< getOutput ]

ghcLinkArgs :: Args
ghcLinkArgs = builder (Ghc LinkHs) ? do
    stage   <- getStage
    way     <- getWay
    pkg     <- getPackage
    libs    <- getPkgDataList DepExtraLibs
    libDirs <- getPkgDataList DepLibDirs
    intLib  <- getIntegerPackage
    gmpLibs <- if stage > Stage0 && intLib == integerGmp
               then do -- TODO: get this data more gracefully
                   let strip = fromMaybe "" . stripPrefix "extra-libraries: "
                   buildInfo <- expr $ readFileLines gmpBuildInfoPath
                   return $ concatMap (words . strip) buildInfo
               else return []
    mconcat [ (Dynamic `wayUnit` way) ?
              pure [ "-shared", "-dynamic", "-dynload", "deploy" ]
            , arg "-no-auto-link-packages"
            ,      nonHsMainPackage pkg  ? arg "-no-hs-main"
            , not (nonHsMainPackage pkg) ? arg "-rtsopts"
            , pure [ "-optl-l" ++           lib | lib <- libs ++ gmpLibs ]
            , pure [ "-optl-L" ++ unifyPath dir | dir <- libDirs ] ]

splitObjectsArgs :: Args
splitObjectsArgs = splitObjects <$> flavour ? do
    expr $ need [ghcSplitPath]
    arg "-split-objs"

findHsDependencies :: Args
findHsDependencies = builder (Ghc FindHsDependencies) ? do
    ways <- getLibraryWays
    mconcat [ arg "-M"
            , commonGhcArgs
            , arg "-include-pkg-deps"
            , arg "-dep-makefile", arg =<< getOutput
            , pure $ concat [ ["-dep-suffix", wayPrefix w] | w <- ways ]
            , getInputs ]

haddockGhcArgs :: Args
haddockGhcArgs = mconcat [ commonGhcArgs, getPkgDataList HsArgs ]

-- Used in ghcBuilderArgs, ghcCBuilderArgs, ghcMBuilderArgs and haddockGhcArgs.
commonGhcArgs :: Args
commonGhcArgs = do
    way  <- getWay
    path <- getBuildPath
    pkg  <- getPackage
    when (pkg == rts) $ do
        context <- getContext
        conf <- expr $ pkgConfFile context
        expr $ need [conf]
    mconcat [ arg "-hisuf", arg $ hisuf way
            , arg "-osuf" , arg $  osuf way
            , arg "-hcsuf", arg $ hcsuf way
            , wayGhcArgs
            , packageGhcArgs
            , includeGhcArgs
            , map ("-optc" ++) <$> getStagedSettingList ConfCcArgs
            , map ("-optP" ++) <$> getStagedSettingList ConfCppArgs
            , map ("-optP" ++) <$> getPkgDataList CppArgs
            , arg "-odir"    , arg path
            , arg "-hidir"   , arg path
            , arg "-stubdir" , arg path ]

-- TODO: Do '-ticky' in all debug ways?
wayGhcArgs :: Args
wayGhcArgs = do
    way <- getWay
    mconcat [ if (Dynamic `wayUnit` way)
              then pure ["-fPIC", "-dynamic"]
              else arg "-static"
            , (Threaded  `wayUnit` way) ? arg "-optc-DTHREADED_RTS"
            , (Debug     `wayUnit` way) ? arg "-optc-DDEBUG"
            , (Profiling `wayUnit` way) ? arg "-prof"
            , (Logging   `wayUnit` way) ? arg "-eventlog"
            , (way == debug || way == debugDynamic) ?
              pure ["-ticky", "-DTICKY_TICKY"] ]

packageGhcArgs :: Args
packageGhcArgs = withHsPackage $ \cabalFile -> do
    pkgId <- expr $ pkgIdentifier cabalFile
    mconcat [ arg "-hide-all-packages"
            , arg "-no-user-package-db"
            , bootPackageDatabaseArgs
            , libraryPackage ? arg ("-this-unit-id " ++ pkgId)
            , map ("-package-id " ++) <$> getPkgDataList DepIds ]

includeGhcArgs :: Args
includeGhcArgs = do
    pkg     <- getPackage
    path    <- getBuildPath
    root    <- getBuildRoot
    context <- getContext
    srcDirs <- getPkgDataList SrcDirs
    autogen <- expr $ autogenPath context
    mconcat [ arg "-i"
            , arg $ "-i" ++ path
            , arg $ "-i" ++ autogen
            , pure [ "-i" ++ pkgPath pkg -/- dir | dir <- srcDirs ]
            , cIncludeArgs
            , arg $      "-I" ++ root -/- generatedDir
            , arg $ "-optc-I" ++ root -/- generatedDir
            , (not $ nonCabalContext context) ?
              pure [ "-optP-include", "-optP" ++ autogen -/- "cabal_macros.h" ] ]

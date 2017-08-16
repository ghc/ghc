module Settings.Builders.Ghc (
    ghcBuilderArgs, ghcMBuilderArgs, haddockGhcArgs, ghcCbuilderArgs
    ) where

import Flavour
import Rules.Gmp
import Settings.Builders.Common

ghcBuilderArgs :: Args
ghcBuilderArgs = (builder (Ghc CompileHs) ||^ builder (Ghc LinkHs)) ? do
    needTouchy
    mconcat [ arg "-Wall"
            , commonGhcArgs
            , splitObjectsArgs
            , ghcLinkArgs
            , builder (Ghc CompileHs) ? arg "-c"
            , getInputs
            , arg "-o", arg =<< getOutput ]

needTouchy :: Expr ()
needTouchy = notStage0 ? do
    maybePath <- expr $ programPath (vanillaContext Stage0 touchy)
    expr . whenJust maybePath $ \path -> need [path]

ghcCbuilderArgs :: Args
ghcCbuilderArgs =
  builder (Ghc CompileCWithGhc) ? do
    way <- getWay
    let ccArgs = [ getPkgDataList CcArgs
                 , getStagedSettingList ConfCcArgs
                 , cIncludeArgs
                 , arg "-Werror"
                 , Dynamic `wayUnit` way ? pure [ "-fPIC", "-DDYNAMIC" ] ]

    mconcat [ arg "-Wall"
            , ghcLinkArgs
            , commonGhcArgs
            , mconcat (map (map ("-optc" ++) <$>) ccArgs)
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
    intLib  <- expr (integerLibrary =<< flavour)
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

ghcMBuilderArgs :: Args
ghcMBuilderArgs = builder (Ghc FindHsDependencies) ? do
    ways <- getLibraryWays
    mconcat [ arg "-M"
            , commonGhcArgs
            , arg "-include-pkg-deps"
            , arg "-dep-makefile", arg =<< getOutput
            , pure $ concat [ ["-dep-suffix", wayPrefix w] | w <- ways ]
            , getInputs ]

haddockGhcArgs :: Args
haddockGhcArgs = mconcat [ commonGhcArgs, getPkgDataList HsArgs ]

-- This is included into ghcBuilderArgs, ghcMBuilderArgs and haddockGhcArgs.
commonGhcArgs :: Args
commonGhcArgs = do
    way     <- getWay
    path    <- getBuildPath
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

-- FIXME: Get rid of to-be-deprecated -this-package-key.
packageGhcArgs :: Args
packageGhcArgs = do
    compId  <- getPkgData ComponentId
    thisArg <- do
        not0 <- notStage0
        unit <- expr $ flag SupportsThisUnitId
        return $ if not0 || unit then "-this-unit-id " else "-this-package-key "
    mconcat [ arg "-hide-all-packages"
            , arg "-no-user-package-db"
            , bootPackageDatabaseArgs
            , libraryPackage ? arg (thisArg ++ compId)
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

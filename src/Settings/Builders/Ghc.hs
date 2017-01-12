module Settings.Builders.Ghc (ghcBuilderArgs, ghcMBuilderArgs, haddockGhcArgs) where

import Flavour
import GHC
import Settings.Builders.Common

ghcBuilderArgs :: Args
ghcBuilderArgs = (builder (Ghc CompileHs) ||^ builder (Ghc LinkHs)) ? do
    needTouchy
    mconcat [ arg "-Wall"
            , commonGhcArgs
            , splitObjectsArgs
            , ghcLinkArgs
            , builder (Ghc CompileHs) ? arg "-c"
            , append =<< getInputs
            , arg "-o", arg =<< getOutput ]

ghcLinkArgs :: Args
ghcLinkArgs = builder (Ghc LinkHs) ? do
    stage   <- getStage
    pkg     <- getPackage
    libs    <- getPkgDataList DepExtraLibs
    libDirs <- getPkgDataList DepLibDirs
    gmpLibs <- if stage > Stage0
               then do -- TODO: get this data more gracefully
                   let strip = fromMaybe "" . stripPrefix "extra-libraries: "
                   buildInfo <- lift $ readFileLines gmpBuildInfoPath
                   return $ concatMap (words . strip) buildInfo
               else return []
    mconcat [ arg "-no-auto-link-packages"
            , nonHsMainPackage pkg ? arg "-no-hs-main"
            , append [ "-optl-l" ++           lib | lib <- libs ++ gmpLibs ]
            , append [ "-optl-L" ++ unifyPath dir | dir <- libDirs ] ]

needTouchy :: ReaderT Target Action ()
needTouchy = notStage0 ? do
    maybePath <- lift $ programPath (vanillaContext Stage0 touchy)
    lift . whenJust maybePath $ \path -> need [path]

splitObjectsArgs :: Args
splitObjectsArgs = splitObjects flavour ? do
    lift $ need [ghcSplit]
    arg "-split-objs"

ghcMBuilderArgs :: Args
ghcMBuilderArgs = builder (Ghc FindHsDependencies) ? do
    ways <- getLibraryWays
    mconcat [ arg "-M"
            , commonGhcArgs
            , arg "-include-pkg-deps"
            , arg "-dep-makefile", arg =<< getOutput
            , append $ concat [ ["-dep-suffix", wayPrefix w] | w <- ways ]
            , append =<< getInputs ]

haddockGhcArgs :: Args
haddockGhcArgs = mconcat [ commonGhcArgs, append =<< getPkgDataList HsArgs ]

-- This is included into ghcBuilderArgs, ghcMBuilderArgs and haddockGhcArgs.
commonGhcArgs :: Args
commonGhcArgs = do
    way     <- getWay
    path    <- getBuildPath
    confCc  <- getSettingList . ConfCcArgs =<< getStage
    confCpp <- getSettingList . ConfCppArgs =<< getStage
    cppArgs <- getPkgDataList CppArgs
    mconcat [ arg "-hisuf", arg $ hisuf way
            , arg "-osuf" , arg $  osuf way
            , arg "-hcsuf", arg $ hcsuf way
            , wayGhcArgs
            , packageGhcArgs
            , includeGhcArgs
            , append $ map ("-optc" ++) confCc
            , append $ map ("-optP" ++) confCpp
            , append $ map ("-optP" ++) cppArgs
            , arg "-odir"    , arg path
            , arg "-hidir"   , arg path
            , arg "-stubdir" , arg path
            , (not . nonHsMainPackage) <$> getPackage ? arg "-rtsopts" ]

-- TODO: Do '-ticky' in all debug ways?
wayGhcArgs :: Args
wayGhcArgs = do
    way <- getWay
    mconcat [ if (Dynamic `wayUnit` way)
              then append ["-fPIC", "-dynamic"]
              else arg "-static"
            , (Threaded  `wayUnit` way) ? arg "-optc-DTHREADED_RTS"
            , (Debug     `wayUnit` way) ? arg "-optc-DDEBUG"
            , (Profiling `wayUnit` way) ? arg "-prof"
            , (Logging   `wayUnit` way) ? arg "-eventlog"
            , (way == debug || way == debugDynamic) ?
              append ["-ticky", "-DTICKY_TICKY"] ]

packageGhcArgs :: Args
packageGhcArgs = do
    pkg       <- getPackage
    compId    <- getPkgData ComponentId
    pkgDepIds <- getPkgDataList DepIds
    -- FIXME: Get rid of to-be-deprecated -this-package-key.
    thisArg <- do
        not0 <- notStage0
        unit <- getFlag SupportsThisUnitId
        return $ if not0 || unit then "-this-unit-id " else "-this-package-key "
    mconcat [ arg "-hide-all-packages"
            , arg "-no-user-package-db"
            , bootPackageDatabaseArgs
            , isLibrary pkg ? arg (thisArg ++ compId)
            , append $ map ("-package-id " ++) pkgDepIds ]

includeGhcArgs :: Args
includeGhcArgs = do
    pkg     <- getPackage
    path    <- getBuildPath
    context <- getContext
    srcDirs <- getPkgDataList SrcDirs
    mconcat [ arg "-i"
            , arg $ "-i" ++ path
            , arg $ "-i" ++ autogenPath context
            , append [ "-i" ++ pkgPath pkg -/- dir | dir <- srcDirs ]
            , cIncludeArgs
            , arg $      "-I" ++ generatedPath
            , arg $ "-optc-I" ++ generatedPath
            , (not $ nonCabalContext context) ?
              append [ "-optP-include"
                     , "-optP" ++ autogenPath context -/- "cabal_macros.h" ] ]

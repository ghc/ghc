module Settings.Builders.Ghc (ghcBuilderArgs, haddockGhcArgs) where

import Hadrian.Haskell.Cabal
import Hadrian.Haskell.Cabal.PackageData as PD

import Flavour
import Packages
import Settings.Builders.Common
import Settings.Warnings

ghcBuilderArgs :: Args
ghcBuilderArgs = mconcat [compileAndLinkHs, compileC, findHsDependencies]

compileAndLinkHs :: Args
compileAndLinkHs = (builder (Ghc CompileHs) ||^ builder (Ghc LinkHs)) ? do
    mconcat [ arg "-Wall"
            , commonGhcArgs
            , splitObjects <$> flavour ? arg "-split-objs"
            , ghcLinkArgs
            , defaultGhcWarningsArgs
            , builder (Ghc CompileHs) ? arg "-c"
            , getInputs
            , arg "-o", arg =<< getOutput ]

compileC :: Args
compileC = builder (Ghc CompileCWithGhc) ? do
    way <- getWay
    let ccArgs = [ getPackageData PD.ccOpts
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
    way     <- getWay
    pkg     <- getPackage
    libs    <- pkg == hp2ps ? pure ["m"]
    intLib  <- getIntegerPackage
    gmpLibs <- notStage0 ? intLib == integerGmp ? pure ["gmp"]
    mconcat [ (Dynamic `wayUnit` way) ?
              pure [ "-shared", "-dynamic", "-dynload", "deploy" ]
            , arg "-no-auto-link-packages"
            ,      nonHsMainPackage pkg  ? arg "-no-hs-main"
            , not (nonHsMainPackage pkg) ? arg "-rtsopts"
            , pure [ "-optl-l" ++           lib | lib <- libs ++ gmpLibs ]
            ]

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
haddockGhcArgs = mconcat [ commonGhcArgs, getPackageData PD.hcOpts ]

-- Used in ghcBuilderArgs, ghcCBuilderArgs, ghcMBuilderArgs and haddockGhcArgs.
commonGhcArgs :: Args
commonGhcArgs = do
    way     <- getWay
    path    <- getBuildPath
    pkg     <- getPackage
    ghcVersion <- expr $ ghcVersionH
    mconcat [ arg "-hisuf", arg $ hisuf way
            , arg "-osuf" , arg $  osuf way
            , arg "-hcsuf", arg $ hcsuf way
            , wayGhcArgs
            , packageGhcArgs
            , includeGhcArgs
            -- when compiling the rts for stage1 or stage2
            -- we do not have the rts in the package db at
            -- the time of builind it.  As such we need to
            -- explicity supply the path to the ghc-version
            -- file, to prevent ghc from trying to open the
            -- rts package from the package db, and failing
            -- over while doing so.
            , (pkg == rts) ? notStage0 ? arg ("-ghcversion-file=" ++ ghcVersion)
            , map ("-optc" ++) <$> getStagedSettingList ConfCcArgs
            , map ("-optP" ++) <$> getStagedSettingList ConfCppArgs
            , map ("-optP" ++) <$> getPackageData PD.cppOpts
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
packageGhcArgs = withHsPackage $ \ctx -> do
    pkgId <- expr $ pkgIdentifier ctx
    mconcat [ arg "-hide-all-packages"
            , arg "-no-user-package-db"
            , packageDatabaseArgs
            , libraryPackage ? arg ("-this-unit-id " ++ pkgId)
            , map ("-package-id " ++) <$> getPackageData PD.depIpIds ]

includeGhcArgs :: Args
includeGhcArgs = do
    pkg     <- getPackage
    path    <- getBuildPath
    root    <- getBuildRoot
    context <- getContext
    srcDirs <- getPackageData PD.srcDirs
    autogen <- expr $ autogenPath context
    mconcat [ arg "-i"
            , arg $ "-i" ++ path
            , arg $ "-i" ++ autogen
            , pure [ "-i" ++ pkgPath pkg -/- dir | dir <- srcDirs ]
            , cIncludeArgs
            , arg $      "-I" ++ root -/- generatedDir
            , arg $ "-optc-I" ++ root -/- generatedDir
            , pure [ "-optP-include", "-optP" ++ autogen -/- "cabal_macros.h" ] ]

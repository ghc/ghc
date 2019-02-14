module Settings.Builders.Ghc (ghcBuilderArgs, haddockGhcArgs) where

import Hadrian.Haskell.Cabal
import Hadrian.Haskell.Cabal.Type

import Flavour
import Packages
import Settings.Builders.Common
import Settings.Warnings
import qualified Context as Context

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
    let ccArgs = [ getContextData ccOpts
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
    pkg     <- getPackage
    libs    <- getContextData extraLibs
    libDirs <- getContextData extraLibDirs
    fmwks   <- getContextData frameworks
    dynamic <- requiresDynamic
    darwin  <- expr osxHost

    -- Relative path from the output (rpath $ORIGIN).
    originPath <- dropFileName <$> getOutput
    context <- getContext
    libPath' <- expr (libPath context)
    distDir <- expr Context.distDir
    let
        distPath = libPath' -/- distDir
        originToLibsDir = makeRelativeNoSysLink originPath distPath
        rpath | darwin = "@loader_path" -/- originToLibsDir
              | otherwise = "$ORIGIN" -/- originToLibsDir

    mconcat [ dynamic ? mconcat
                [ arg "-dynamic"
                -- TODO what about windows?
                , isLibrary pkg ? pure [ "-shared", "-dynload", "deploy" ]
                , notStage0 ?
                  hostSupportsRPaths ? arg ("-optl-Wl,-rpath," ++ rpath)
                ]
            , arg "-no-auto-link-packages"
            ,      nonHsMainPackage pkg  ? arg "-no-hs-main"
            , not (nonHsMainPackage pkg) ? arg "-rtsopts"
            , pure [ "-l" ++ lib    | lib <-    libs    ]
            , pure [ "-L" ++ libDir | libDir <- libDirs ]
            , darwin ? pure (concat [ ["-framework", fmwk] | fmwk <- fmwks ])
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
haddockGhcArgs = mconcat [ commonGhcArgs
                         , getContextData hcOpts
                         , ghcWarningsArgs ]

-- | Common GHC command line arguments used in 'ghcBuilderArgs',
-- 'ghcCBuilderArgs', 'ghcMBuilderArgs' and 'haddockGhcArgs'.
commonGhcArgs :: Args
commonGhcArgs = do
    way  <- getWay
    path <- getBuildPath
    ghcVersion <- expr ghcVersionH
    mconcat [ arg "-hisuf", arg $ hisuf way
            , arg "-osuf" , arg $  osuf way
            , arg "-hcsuf", arg $ hcsuf way
            , wayGhcArgs
            , packageGhcArgs
            , includeGhcArgs
            -- When compiling RTS for Stage1 or Stage2 we do not have it (yet)
            -- in the package database. We therefore explicity supply the path
            -- to the @ghc-version@ file, to prevent GHC from trying to open the
            -- RTS package in the package database and failing.
            , package rts ? notStage0 ? arg ("-ghcversion-file=" ++ ghcVersion)
            , map ("-optc" ++) <$> getStagedSettingList ConfCcArgs
            , map ("-optP" ++) <$> getStagedSettingList ConfCppArgs
            , map ("-optP" ++) <$> getContextData cppOpts
            , arg "-outputdir", arg path ]

-- TODO: Do '-ticky' in all debug ways?
wayGhcArgs :: Args
wayGhcArgs = do
    way <- getWay
    dynamic <- requiresDynamic
    mconcat [ if dynamic
                then pure ["-fPIC", "-dynamic"]
                else arg "-static"
            , (Threaded  `wayUnit` way) ? arg "-optc-DTHREADED_RTS"
            , (Debug     `wayUnit` way) ? arg "-optc-DDEBUG"
            , (Profiling `wayUnit` way) ? arg "-prof"
            , (Logging   `wayUnit` way) ? arg "-eventlog"
            , (way == debug || way == debugDynamic) ?
              pure ["-ticky", "-DTICKY_TICKY"] ]

packageGhcArgs :: Args
packageGhcArgs = do
    package <- getPackage
    pkgId   <- expr $ pkgIdentifier package
    mconcat [ arg "-hide-all-packages"
            , arg "-no-user-package-db"
            , packageDatabaseArgs
            , libraryPackage ? arg ("-this-unit-id " ++ pkgId)
            , map ("-package-id " ++) <$> getContextData depIds ]

includeGhcArgs :: Args
includeGhcArgs = do
    pkg     <- getPackage
    path    <- getBuildPath
    root    <- getBuildRoot
    context <- getContext
    srcDirs <- getContextData srcDirs
    autogen <- expr $ autogenPath context
    let cabalMacros = autogen -/- "cabal_macros.h"
    expr $ need [cabalMacros]
    mconcat [ arg "-i"
            , arg $ "-i" ++ path
            , arg $ "-i" ++ autogen
            , pure [ "-i" ++ pkgPath pkg -/- dir | dir <- srcDirs ]
            , cIncludeArgs
            , arg $      "-I" ++ root -/- generatedDir
            , arg $ "-optc-I" ++ root -/- generatedDir
            , pure ["-optP-include", "-optP" ++ cabalMacros] ]

-- Check if building dynamically is required. GHC is a special case that needs
-- to be built dynamically if any of the RTS ways is dynamic.
requiresDynamic :: Expr Bool
requiresDynamic = wayUnit Dynamic <$> getWay
    -- TODO This logic has been reverted as the dynamic build is broken.
    --      See #15837.
    --
    -- pkg <- getPackage
    -- way <- getWay
    -- rtsWays <- getRtsWays
    -- let
    --     dynRts = any (Dynamic `wayUnit`) rtsWays
    --     dynWay = Dynamic `wayUnit` way
    -- return $ if pkg == ghc
    --             then dynRts || dynWay
    --             else dynWay

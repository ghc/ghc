{-# LANGUAGE ScopedTypeVariables #-}

module Settings.Builders.Ghc (ghcBuilderArgs, haddockGhcArgs) where

import Hadrian.Haskell.Cabal
import Hadrian.Haskell.Cabal.Type

import Flavour
import Packages
import Settings.Builders.Common
import Settings.Warnings
import qualified Context as Context
import Rules.Libffi (libffiName)
import qualified Data.Set as Set
import System.Directory
import Data.Version.Extra

ghcBuilderArgs :: Args
ghcBuilderArgs = mconcat
  [ compileAndLinkHs, compileC, compileCxx, findHsDependencies
  , toolArgs ]

toolArgs :: Args
toolArgs = do
  builder (Ghc ToolArgs) ? mconcat
              [ packageGhcArgs
              , includeGhcArgs
              , map ("-optc" ++) <$> getStagedCCFlags
              , map ("-optP" ++) <$> getContextData cppOpts
              , getContextData hcOpts
              ]

compileAndLinkHs :: Args
compileAndLinkHs = (builder (Ghc CompileHs) ||^ builder (Ghc LinkHs)) ? do
    ways <- getLibraryWays
    useColor <- shakeColor <$> expr getShakeOptions
    let hasVanilla = elem vanilla ways
        hasDynamic = elem dynamic ways
    mconcat [ arg "-Wall"
            , arg "-Wcompat"
            , not useColor ? builder (Ghc CompileHs) ?
              -- N.B. Target.trackArgument ignores this argument from the
              -- input hash to avoid superfluous recompilation, avoiding
              -- #18672.
              arg "-fdiagnostics-color=never"
            , (hasVanilla && hasDynamic) ? builder (Ghc CompileHs) ?
              platformSupportsSharedLibs ? way vanilla ?
              arg "-dynamic-too"
            , commonGhcArgs
            , ghcLinkArgs
            , defaultGhcWarningsArgs
            , builder (Ghc CompileHs) ? arg "-c"
            , getInputs
            , arg "-o", arg =<< getOutput ]

compileC :: Args
compileC = builder (Ghc CompileCWithGhc) ? do
    way <- getWay
    let ccArgs = [ getContextData ccOpts
                 , getStagedCCFlags
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

compileCxx :: Args
compileCxx = builder (Ghc CompileCppWithGhc) ? do
    way <- getWay
    let ccArgs = [ getContextData cxxOpts
                 , getStagedCCFlags
                 , cIncludeArgs
                 , Dynamic `wayUnit` way ? pure [ "-fPIC", "-DDYNAMIC" ] ]
    mconcat [ arg "-Wall"
            , ghcLinkArgs
            , commonGhcArgs
            , mconcat (map (map ("-optcxx" ++) <$>) ccArgs)
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
    way     <- getWay

    -- Relative path from the output (rpath $ORIGIN).
    originPath <- dropFileName <$> getOutput
    context <- getContext
    distPath <- expr (Context.distDynDir context)

    useSystemFfi <- expr (flag UseSystemFfi)
    buildPath <- getBuildPath
    libffiName' <- libffiName
    debugged <- buildingCompilerStage' . ghcDebugged =<< expr flavour

    osxTarget <- expr isOsxTarget
    winTarget <- expr isWinTarget

    let
        dynamic = Dynamic `wayUnit` way
        originToLibsDir = makeRelativeNoSysLink originPath distPath
        rpath
            -- Programs will end up in the bin dir ($ORIGIN) and will link to
            -- libraries in the lib dir.
            | isProgram pkg = metaOrigin -/- originToLibsDir
            -- libraries will all end up in the lib dir, so just use $ORIGIN
            | otherwise     = metaOrigin
            where
                metaOrigin | osxTarget = "@loader_path"
                           | otherwise = "$ORIGIN"

        -- TODO: an alternative would be to generalize by linking with extra
        -- bundled libraries, but currently the rts is the only use case. It is
        -- a special case when `useSystemFfi == True`: the ffi library files
        -- are not actually bundled with the rts. Perhaps ffi should be part of
        -- rts's extra libraries instead of extra bundled libraries in that
        -- case. Care should be take as to not break the make build.
        rtsFfiArg = package rts ? not useSystemFfi ? mconcat
            [ arg ("-L" ++ buildPath)
            , arg ("-l" ++ libffiName')
            ]

        -- This is the -rpath argument that is required for the bindist scenario
        -- to work. Indeed, when you install a bindist, the actual executables
        -- end up nested somewhere under $libdir, with the wrapper scripts
        -- taking their place in $bindir, and 'rpath' therefore doesn't seem
        -- to give us the right paths for such a case.
        -- TODO: Could we get away with just one rpath...?
        bindistRpath = "$ORIGIN" -/- ".." -/- ".." -/- originToLibsDir

    mconcat [ dynamic ? mconcat
                [ arg "-dynamic"
                -- TODO what about windows?
                , isLibrary pkg ? pure [ "-shared", "-dynload", "deploy" ]
                , notStage0 ? targetSupportsRPaths ? mconcat
                      [ arg ("-optl-Wl,-rpath," ++ rpath)
                      , isProgram pkg ? arg ("-optl-Wl,-rpath," ++ bindistRpath)
                      -- The darwin and Windows linkers don't support/require the -zorigin option
                      , not (osxTarget || winTarget) ? arg "-optl-Wl,-zorigin"
                      -- We set RPATH directly (relative to $ORIGIN). There's
                      -- no reason for GHC to inject further RPATH entries.
                      -- See #19485.
                      , arg "-fno-use-rpaths"
                      ]
                ]
            , arg "-no-auto-link-packages"
            ,       nonHsMainPackage pkg  ? arg "-no-hs-main"
            , (not (nonHsMainPackage pkg) && not (isLibrary pkg)) ? arg "-rtsopts"
            , pure [ "-l" ++ lib    | lib    <- libs    ]
            , pure [ "-L" ++ libDir | libDir <- libDirs ]
            , rtsFfiArg
            , osxTarget ? pure (concat [ ["-framework", fmwk] | fmwk <- fmwks ])
            , debugged ? packageOneOf [ghc, iservProxy, iserv, remoteIserv] ?
              arg "-debug"
            ]

findHsDependencies :: Args
findHsDependencies = builder (Ghc FindHsDependencies) ? do
    ways <- getLibraryWays
    mconcat [ arg "-M"
            , arg "-include-cpp-deps"
            , commonGhcArgs
            , defaultGhcWarningsArgs
            , arg "-include-pkg-deps"
            , arg "-dep-makefile", arg =<< getOutput
            , pure $ concat [ ["-dep-suffix", wayPrefix w] | w <- Set.toList ways ]
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
    useColor <- shakeColor <$> expr getShakeOptions
    mconcat [ arg "-hisuf", arg $ hisuf way
            , arg "-osuf" , arg $  osuf way
            , arg "-hcsuf", arg $ hcsuf way
            , wayGhcArgs
            , packageGhcArgs
            , includeGhcArgs
            -- When compiling RTS for Stage1 or Stage2 we do not have it (yet)
            -- in the package database. We therefore explicitly supply the path
            -- to the @ghc-version@ file, to prevent GHC from trying to open the
            -- RTS package in the package database and failing.
            , package rts ? notStage0 ? arg "-ghcversion-file=rts/include/ghcversion.h"
            , map ("-optc" ++) <$> getStagedCCFlags
            , map ("-optP" ++) <$> getContextData cppOpts
            , arg "-outputdir", arg path
              -- we need to enable color explicitly because the output is
              -- captured to be displayed after the failed command line in case
              -- of error (#20490). GHC detects that it doesn't output to a
              -- terminal and it disables colors if we don't do this.
            , useColor ?
              -- N.B. Target.trackArgument ignores this argument from the
              -- input hash to avoid superfluous recompilation, avoiding
              -- #18672.
              arg "-fdiagnostics-color=always"
            -- Important this is last.. as these options can override the default options
            , getContextData hcOpts
            ]

-- TODO: Do '-ticky' in all debug ways?
wayGhcArgs :: Args
wayGhcArgs = do
    way <- getWay
    mconcat [ if Dynamic `wayUnit` way
                then pure ["-fPIC", "-dynamic"]
                else arg "-static"
            , (Profiling `wayUnit` way) ? arg "-prof"
            , (way == debug || way == debugDynamic) ? arg "-ticky"
            , wayCcArgs
              -- We must pass CPP flags via -optc as well to ensure that they
              -- are passed to the preprocessor when, e.g., compiling Cmm
              -- sources.
            , map ("-optc"++) <$> wayCcArgs
            ]

-- | Args related to correct handling of packages, such as setting
-- -this-unit-id and passing -package-id for dependencies
packageGhcArgs :: Args
packageGhcArgs = do
    package <- getPackage
    stage <- getStage
    ghc_ver <- readVersion <$> (expr . ghcVersionStage =<< getStage)
    -- ROMES: Until the boot compiler no longer needs ghc's
    -- unit-id to be "ghc", the stage0 compiler must be built
    -- with `-this-unit-id ghc`, while the wired-in unit-id of
    -- ghc is correctly set to the unit-id we'll generate for
    -- stage1 (set in generateConfigHs in Rules.Generate).
    --
    -- However, we don't need to set the unit-id of "ghc" to "ghc" when
    -- building stage0 because we have a flag in compiler/ghc.cabal.in that is
    -- sets `-this-unit-id ghc` when hadrian is building stage0, which will
    -- overwrite this one.
    pkgId   <- expr $ pkgUnitId stage package
    pkgName <- expr $ pkgPackageName package
    mconcat [ arg "-hide-all-packages"
            , arg "-no-user-package-db"
            , arg "-package-env -"
            , packageDatabaseArgs
            -- We want to pass -this-unit-id for executables as well for multi-repl to
            -- work with executable packages but this is buggy on GHC-9.0.2
            , (isLibrary package || (ghc_ver >= makeVersion [9,2,1])) ? mconcat
                [ arg ("-this-unit-id " ++ pkgId)
                , arg ("-this-package-name " ++ pkgName)
                ]
            , map ("-package-id " ++) <$> getContextData depIds ]

includeGhcArgs :: Args
includeGhcArgs = do
    pkg     <- getPackage
    path    <- exprIO . makeAbsolute =<< getBuildPath
    context <- getContext
    srcDirs <- getContextData srcDirs
    abSrcDirs <- exprIO $ mapM makeAbsolute [ (pkgPath pkg -/- dir) | dir <- srcDirs ]
    autogen <- expr (autogenPath context)
    cautogen <-  exprIO (makeAbsolute autogen)
    let cabalMacros = autogen -/- "cabal_macros.h"
    expr $ need [cabalMacros]
    mconcat [ arg "-i"
            , arg $ "-i" ++ path
            , arg $ "-i" ++ cautogen
            , pure [ "-i" ++ d | d <- abSrcDirs ]
            , cIncludeArgs
            , pure ["-optP-include", "-optP" ++ cabalMacros] ]

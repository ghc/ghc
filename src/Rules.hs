module Rules (topLevelTargets, buildRules) where

import Data.Foldable

import Base
import Context hiding (stage, package, way)
import Expression
import GHC
import Rules.Compile
import Rules.Data
import Rules.Dependencies
import Rules.Documentation
import Rules.Generate
import Rules.Cabal
import Rules.Gmp
import Rules.Libffi
import Rules.Library
import Rules.Perl
import Rules.Program
import Rules.Register
import Rules.Setup
import Settings

allStages :: [Stage]
allStages = [minBound ..]

-- | 'need' all top-level build targets
topLevelTargets :: Rules ()
topLevelTargets = do

    want $ Rules.Generate.installTargets

    -- TODO: do we want libffiLibrary to be a top-level target?

    action $ do -- TODO: Add support for all rtsWays
        rtsLib    <- pkgLibraryFile Stage1 rts vanilla
        rtsThrLib <- pkgLibraryFile Stage1 rts threaded
        need [ rtsLib, rtsThrLib ]

    for_ allStages $ \stage ->
        for_ (knownPackages \\ [rts, libffi]) $ \pkg -> action $ do
            let context = vanillaContext stage pkg
            activePackages <- interpretInContext context getPackages
            when (pkg `elem` activePackages) $
                if isLibrary pkg
                then do -- build a library
                    ways <- interpretInContext context getLibraryWays
                    libs <- traverse (pkgLibraryFile stage pkg) ways
                    docs <- interpretInContext context buildHaddock
                    need $ libs ++ [ pkgHaddockFile pkg | docs && stage == Stage1 ]
                else do -- otherwise build a program
                    need [ fromJust $ programPath stage pkg ] -- TODO: drop fromJust

packageRules :: Rules ()
packageRules = do
    -- We cannot register multiple GHC packages in parallel. Also we cannot run
    -- GHC when the package database is being mutated by "ghc-pkg". This is a
    -- classic concurrent read exclusive write (CREW) conflict.
    let maxConcurrentReaders = 1000
    packageDb <- newResource "package-db" maxConcurrentReaders
    let readPackageDb  = [(packageDb, 1)]
        writePackageDb = [(packageDb, maxConcurrentReaders)]

    let contexts        = liftM3 Context        allStages knownPackages allWays
        vanillaContexts = liftM2 vanillaContext allStages knownPackages

    for_ contexts $ mconcat
        [ compilePackage readPackageDb
        , buildPackageLibrary ]

    for_ vanillaContexts $ mconcat
        [ buildPackageData
        , buildPackageDependencies readPackageDb
        , buildPackageDocumentation
        , generatePackageCode ]

    for_ allStages $ \stage ->
        for_ knownPackages $ \package -> do
            let context = vanillaContext stage package
            buildProgram                             context
            registerPackage           writePackageDb context

buildRules :: Rules ()
buildRules = do
    cabalRules
    generateRules
    copyRules
    gmpRules
    libffiRules
    perlScriptRules
    setupRules
    packageRules

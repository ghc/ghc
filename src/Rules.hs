module Rules (topLevelTargets, buildRules) where

import Data.Foldable

import Base
import Expression
import GHC
import Rules.Compile
import Rules.Data
import Rules.Dependencies
import Rules.Documentation
import Rules.Generate
import Rules.Resources
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
    resources <- resourceRules
    for_ allStages $ \stage ->
        for_ knownPackages $ \package -> do
            let context = vanillaContext stage package
            compilePackage            resources context
            buildPackageData                    context
            buildPackageDependencies  resources context
            buildPackageDocumentation           context
            generatePackageCode                 context
            buildPackageLibrary                 context
            buildProgram                        context
            registerPackage           resources context

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

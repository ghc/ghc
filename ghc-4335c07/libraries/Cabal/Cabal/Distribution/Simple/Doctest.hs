{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.Doctest
-- Copyright   :  Moritz Angermann 2017
-- License     :  BSD3
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- This module deals with the @doctest@ command.

-- Note: this module is modelled after Distribution.Simple.Haddock

module Distribution.Simple.Doctest (
  doctest
  ) where

import Prelude ()
import Distribution.Compat.Prelude

import qualified Distribution.Simple.GHC   as GHC
import qualified Distribution.Simple.GHCJS as GHCJS

-- local
import Distribution.PackageDescription as PD hiding (Flag)
import Distribution.Simple.Compiler hiding (Flag)
import Distribution.Simple.Program.GHC
import Distribution.Simple.Program
import Distribution.Simple.PreProcess
import Distribution.Simple.Setup
import Distribution.Simple.Build
import Distribution.Simple.LocalBuildInfo hiding (substPathTemplate)
import Distribution.Simple.Register              (internalPackageDBPath)
import Distribution.Simple.BuildPaths
import Distribution.Simple.Utils
import Distribution.System
import Distribution.Utils.NubList
import Distribution.Version
import Distribution.Verbosity

-- -----------------------------------------------------------------------------
-- Types

-- | A record that represents the arguments to the doctest executable.
data DoctestArgs = DoctestArgs {
    argTargets :: [FilePath]
    -- ^ Modules to process
  , argGhcOptions :: Flag (GhcOptions, Version)
} deriving (Show, Generic)

-- -----------------------------------------------------------------------------
-- Doctest support

doctest :: PackageDescription
        -> LocalBuildInfo
        -> [PPSuffixHandler]
        -> DoctestFlags
        -> IO ()
doctest pkg_descr lbi suffixes doctestFlags = do
  let verbosity     = flag doctestVerbosity
      distPref      = flag doctestDistPref
      flag f        = fromFlag $ f doctestFlags
      tmpFileOpts   = defaultTempFileOptions
      lbi'          = lbi { withPackageDB = withPackageDB lbi
                            ++ [SpecificPackageDB (internalPackageDBPath lbi distPref)] }

  (doctestProg, _version, _) <-
    requireProgramVersion verbosity doctestProgram
      (orLaterVersion (mkVersion [0,11,3])) (withPrograms lbi)

  withAllComponentsInBuildOrder pkg_descr lbi $ \component clbi -> do
     componentInitialBuildSteps distPref pkg_descr lbi clbi verbosity
     preprocessComponent pkg_descr component lbi clbi False verbosity suffixes

     case component of
       CLib lib -> do
         withTempDirectoryEx verbosity tmpFileOpts (buildDir lbi) "tmp" $
           \tmp -> do
             inFiles <- map snd <$> getLibSourceFiles verbosity lbi lib clbi
             args    <- mkDoctestArgs verbosity tmp lbi' clbi inFiles (libBuildInfo lib)
             runDoctest verbosity (compiler lbi) (hostPlatform lbi) doctestProg args
       CExe exe -> do
         withTempDirectoryEx verbosity tmpFileOpts (buildDir lbi) "tmp" $
           \tmp -> do
             inFiles <- map snd <$> getExeSourceFiles verbosity lbi exe clbi
             args    <- mkDoctestArgs verbosity tmp lbi' clbi inFiles (buildInfo exe)
             runDoctest verbosity (compiler lbi) (hostPlatform lbi) doctestProg args
       CFLib _  -> return () -- do not doctest foreign libs
       CTest _  -> return () -- do not doctest tests
       CBench _ -> return () -- do not doctest benchmarks

-- -----------------------------------------------------------------------------
-- Contributions to DoctestArgs (see also Haddock.hs for very similar code).

componentGhcOptions :: Verbosity -> LocalBuildInfo
                 -> BuildInfo -> ComponentLocalBuildInfo -> FilePath
                 -> GhcOptions
componentGhcOptions verbosity lbi bi clbi odir =
  let f = case compilerFlavor (compiler lbi) of
            GHC   -> GHC.componentGhcOptions
            GHCJS -> GHCJS.componentGhcOptions
            _     -> error $
                       "Distribution.Simple.Doctest.componentGhcOptions:" ++
                       "doctest only supports GHC and GHCJS"
  in f verbosity lbi bi clbi odir

mkDoctestArgs :: Verbosity
              -> FilePath
              -> LocalBuildInfo
              -> ComponentLocalBuildInfo
              -> [FilePath]
              -> BuildInfo
              -> IO DoctestArgs
mkDoctestArgs verbosity tmp lbi clbi inFiles bi = do
  let vanillaOpts = (componentGhcOptions normal lbi bi clbi (buildDir lbi))
        { ghcOptOptimisation = mempty -- no optimizations when runnign doctest
        -- disable -Wmissing-home-modules
        , ghcOptWarnMissingHomeModules = mempty
        -- clear out ghc-options: these are likely not meant for doctest.
        -- If so, should be explicitly specified via doctest-ghc-options: again.
        , ghcOptExtra   = mempty
        , ghcOptCabal   = toFlag False

        , ghcOptObjDir  = toFlag tmp
        , ghcOptHiDir   = toFlag tmp
        , ghcOptStubDir = toFlag tmp }
      sharedOpts = vanillaOpts
        { ghcOptDynLinkMode = toFlag GhcDynamicOnly
        , ghcOptFPic        = toFlag True
        , ghcOptHiSuffix    = toFlag "dyn_hi"
        , ghcOptObjSuffix   = toFlag "dyn_o"
        , ghcOptExtra       = toNubListR (hcSharedOptions GHC bi)}
  opts <- if withVanillaLib lbi
          then return vanillaOpts
          else if withSharedLib lbi
          then return sharedOpts
          else die' verbosity $ "Must have vanilla or shared lirbaries "
               ++ "enabled in order to run doctest"
  ghcVersion <- maybe (die' verbosity "Compiler has no GHC version")
                      return
                      (compilerCompatVersion GHC (compiler lbi))
  return $ DoctestArgs
    { argTargets = inFiles
    , argGhcOptions = toFlag (opts, ghcVersion)
    }


-- -----------------------------------------------------------------------------
-- Call doctest with the specified arguments.
runDoctest :: Verbosity
           -> Compiler
           -> Platform
           -> ConfiguredProgram
           -> DoctestArgs
           -> IO ()
runDoctest verbosity comp platform doctestProg args = do
  renderArgs verbosity comp platform args $
    \(flags, files) -> do
      runProgram verbosity doctestProg (flags <> files)

renderArgs :: Verbosity
           -> Compiler
           -> Platform
           -> DoctestArgs
           -> (([String],[FilePath]) -> IO a)
           -> IO a
renderArgs _verbosity comp platform args k = do
  k (flags, argTargets args)
  where
    flags :: [String]
    flags  = mconcat
      [ pure "--no-magic" -- disable doctests automagic discovery heuristics
      , pure "-fdiagnostics-color=never" -- disable ghc's color diagnostics.
      , [ opt | (opts, _ghcVer) <- flagToList (argGhcOptions args)
              , opt <- renderGhcOptions comp platform opts ]
      ]

-- ------------------------------------------------------------------------------
-- Boilerplate Monoid instance.
instance Monoid DoctestArgs where
    mempty = gmempty
    mappend = (<>)

instance Semigroup DoctestArgs where
    (<>) = gmappend

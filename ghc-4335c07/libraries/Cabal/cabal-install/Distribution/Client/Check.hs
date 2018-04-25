{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Client.Check
-- Copyright   :  (c) Lennart Kolmodin 2008
-- License     :  BSD-like
--
-- Maintainer  :  kolmodin@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Check a package for common mistakes
--
-----------------------------------------------------------------------------
module Distribution.Client.Check (
    check
  ) where

import Control.Monad ( when, unless )

import Distribution.PackageDescription.Parsec ( readGenericPackageDescription )
import Distribution.PackageDescription.Check
import Distribution.PackageDescription.Configuration
         ( flattenPackageDescription )
import Distribution.Verbosity
         ( Verbosity )
import Distribution.Simple.Utils
         ( defaultPackageDesc, wrapText )

check :: Verbosity -> IO Bool
check verbosity = do
    pdfile <- defaultPackageDesc verbosity
    ppd <- readGenericPackageDescription verbosity pdfile
    -- flatten the generic package description into a regular package
    -- description
    -- TODO: this may give more warnings than it should give;
    --       consider two branches of a condition, one saying
    --          ghc-options: -Wall
    --       and the other
    --          ghc-options: -Werror
    --      joined into
    --          ghc-options: -Wall -Werror
    --      checkPackages will yield a warning on the last line, but it
    --      would not on each individual branch.
    --      Hovever, this is the same way hackage does it, so we will yield
    --      the exact same errors as it will.
    let pkg_desc = flattenPackageDescription ppd
    ioChecks <- checkPackageFiles pkg_desc "."
    let packageChecks = ioChecks ++ checkPackage ppd (Just pkg_desc)
        buildImpossible = [ x | x@PackageBuildImpossible {} <- packageChecks ]
        buildWarning    = [ x | x@PackageBuildWarning {}    <- packageChecks ]
        distSuspicious  = [ x | x@PackageDistSuspicious {}  <- packageChecks ]
                          ++ [ x | x@PackageDistSuspiciousWarn {}  <- packageChecks ]
        distInexusable  = [ x | x@PackageDistInexcusable {} <- packageChecks ]

    unless (null buildImpossible) $ do
        putStrLn "The package will not build sanely due to these errors:"
        printCheckMessages buildImpossible

    unless (null buildWarning) $ do
        putStrLn "The following warnings are likely to affect your build negatively:"
        printCheckMessages buildWarning

    unless (null distSuspicious) $ do
        putStrLn "These warnings may cause trouble when distributing the package:"
        printCheckMessages distSuspicious

    unless (null distInexusable) $ do
        putStrLn "The following errors will cause portability problems on other environments:"
        printCheckMessages distInexusable

    let isDistError (PackageDistSuspicious     {}) = False
        isDistError (PackageDistSuspiciousWarn {}) = False
        isDistError _                              = True
        isCheckError (PackageDistSuspiciousWarn {}) = False
        isCheckError _                              = True
        errors = filter isDistError packageChecks

    unless (null errors) $
        putStrLn "Hackage would reject this package."

    when (null packageChecks) $
        putStrLn "No errors or warnings could be found in the package."

    return (not . any isCheckError $ packageChecks)

  where
    printCheckMessages = mapM_ (putStrLn . format . explanation)
    format = wrapText . ("* "++)

--
-- Haddock - A Haskell Documentation Tool
--
-- (c) Simon Marlow 2003
--


module Haddock.GHC (
  startGhc,
  loadPackages,
  module Haddock.GHC.Typecheck,
  module Haddock.GHC.Utils
) where


import Haddock.GHC.Typecheck
import Haddock.GHC.Utils
import Haddock.Exception
import Haddock.Options

import Data.Foldable (foldlM)
import Data.Maybe
import Control.Monad

import GHC
import DynFlags hiding (Option)
import Packages hiding (package)
import StaticFlags


-- | Start a GHC session with the -haddock flag set. Also turn off 
-- compilation and linking.  
startGhc :: String -> [String] -> IO (Session, DynFlags)
startGhc libDir flags = do
  restFlags <- parseStaticFlags flags
  session   <- newSession (Just libDir)
  dynflags  <- getSessionDynFlags session
  let dynflags' = dopt_set dynflags Opt_Haddock
  let dynflags'' = dynflags' {
      hscTarget = HscNothing,
      ghcMode   = CompManager,
      ghcLink   = NoLink
    }
  dynflags''' <- parseGhcFlags dynflags'' restFlags flags
  setSessionDynFlags session dynflags'''
  return (session, dynflags''')


-- | Expose the list of packages to GHC. Then initialize GHC's package state
-- and get the name of the actually loaded packages matching the supplied 
-- list of packages. The matching packages might be newer versions of the 
-- supplied ones. For each matching package, return its InstalledPackageInfo. 

loadPackages :: Session -> [String] -> IO [InstalledPackageInfo]

-- It would be better to try to get the "in scope" packages from GHC instead.
-- This would make the -use-package flag unnecessary. But currently it 
-- seems all you can get from the GHC api is all packages that are linked in 
-- (i.e the closure of the "in scope" packages).

loadPackages session pkgStrs = do

  -- expose the packages 

  dfs <- getSessionDynFlags session
  let dfs' = dfs { packageFlags = packageFlags dfs ++ map ExposePackage pkgStrs }
  setSessionDynFlags session dfs'

  -- try to parse the packages and get their names, without versions
  pkgNames <- mapM (handleParse . unpackPackageId . stringToPackageId) pkgStrs

  -- init GHC's package state
  (dfs'', depPackages) <- initPackages dfs'

  -- compute the pkgIds of the loaded packages matching the 
  -- supplied ones
  
  let depPkgs = map (fromJust . unpackPackageId) depPackages      
      matchingPackages = [ mkPackageId pkg | pkg <- depPkgs, 
                           pkgName pkg `elem` pkgNames ]

  -- get InstalledPackageInfos for each package
  let pkgInfos = map (getPackageDetails (pkgState dfs'')) matchingPackages

  return pkgInfos
  where
    handleParse (Just pkg) = return (pkgName pkg)
    handleParse Nothing = throwE "Could not parse package identifier"


-- | Try to parse dynamic GHC flags
parseGhcFlags dynflags flags origFlags = do
  (dynflags', rest) <- parseDynamicFlags dynflags flags
  if not (null rest)
    then throwE ("Couldn't parse GHC options: " ++ (unwords origFlags))
    else return dynflags'

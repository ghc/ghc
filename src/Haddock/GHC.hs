--
-- Haddock - A Haskell Documentation Tool
--
-- (c) Simon Marlow 2003
--


module Haddock.GHC (
  startGhc,
  loadPackages,
  tryParseStaticFlags,
  parseGhcFlags,
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
startGhc :: String -> IO (Session, DynFlags)
startGhc libDir = do
  session <- newSession (Just libDir)
  flags   <- getSessionDynFlags session
  let flags' = dopt_set flags Opt_Haddock
  let flags'' = flags' {
      hscTarget = HscNothing,
      ghcMode   = CompManager,
      ghcLink   = NoLink
    }
  setSessionDynFlags session flags''
  return (session, flags'')


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


-- | Filter out the GHC specific flags and try to parse and set them as static 
-- flags. Return a list of flags that couldn't be parsed. 
tryParseStaticFlags flags = do
  let ghcFlags = [ str | Flag_GhcFlag str <- flags ]
  parseStaticFlags ghcFlags


-- | Try to parse dynamic GHC flags
parseGhcFlags session ghcFlags = do
  dflags <- getSessionDynFlags session
  foldlM parseFlag dflags (map words ghcFlags)
  where 
    -- try to parse a flag as either a dynamic or static GHC flag
    parseFlag dynflags ghcFlag = do
      (dynflags', rest) <- parseDynamicFlags dynflags ghcFlag
      when (rest == ghcFlag) $
          throwE ("Couldn't parse GHC flag: " ++ (unwords ghcFlag))           
      return dynflags'

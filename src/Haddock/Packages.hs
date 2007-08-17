--
-- Haddock - A Haskell Documentation Tool
--
-- (c) Simon Marlow 2003
--


module Haddock.Packages (
  HaddockPackage(..),
  initAndReadPackages,
  combineDocEnvs
) where


import Haddock.Types
import Haddock.Exception
import Haddock.InterfaceFile

import Data.Maybe
import qualified Data.Map as Map
import Control.Monad
import Control.Exception
import System.Directory

import GHC
import DynFlags
import Module
import Packages


-- | Represents the installed Haddock information for a package.
-- This is basically the contents of the .haddock file, the path
-- to the html files and the list of modules in the package
data HaddockPackage = HaddockPackage {
  pdModules  :: [Module],
  pdDocEnv   :: DocEnv,
  pdHtmlPath :: FilePath
}


-- | Expose the list of packages to GHC. Then initialize GHC's package state
-- and get the name of the actually loaded packages matching the supplied 
-- list of packages. The matching packages might be newer versions of the 
-- supplied ones. For each matching package, try to read its installed Haddock
-- information.
--
-- It would be better to try to get the "in scope" packages from GHC instead.
-- This would make the -use-package flag unnecessary. But currently it 
-- seems all you can get from the GHC api is all packages that are linked in 
-- (i.e the closure of the "in scope" packages).
initAndReadPackages :: Session -> [String] -> IO [HaddockPackage] 
initAndReadPackages session pkgStrs = do

  -- expose the packages 

  dfs <- getSessionDynFlags session
  let dfs' = dfs { packageFlags = packageFlags dfs ++ map ExposePackage pkgStrs }
  setSessionDynFlags session dfs'

  -- try to parse the packages and get their names, without versions
  pkgNames <- mapM (handleParse . unpackPackageId . stringToPackageId) pkgStrs

  -- init GHC's package state
  (_, depPackages) <- initPackages dfs'

  -- compute the pkgIds of the loaded packages matching the 
  -- supplied ones
  
  let depPkgs = map (fromJust . unpackPackageId) depPackages      
      matchingPackages = [ mkPackageId pkg | pkg <- depPkgs, 
                           pkgName pkg `elem` pkgNames ]

  -- read the Haddock information for the matching packages
  getPackages session matchingPackages
  where
    handleParse (Just pkg) = return (pkgName pkg)
    handleParse Nothing = throwE "Could not parse package identifier"


-- | Try to create a HaddockPackage for each package.
-- Print a warning on stdout if a HaddockPackage could not be created.
getPackages :: Session -> [PackageId] -> IO [HaddockPackage]
getPackages session packages = do

  -- get InstalledPackageInfos for each package
  dynflags <- getSessionDynFlags session
  let pkgInfos = map (getPackageDetails (pkgState dynflags)) packages

  -- try to read the installed haddock information (.haddock interface file and
  -- html path) for the packages
  liftM catMaybes $ mapM tryGetPackage pkgInfos
  where
    -- try to get a HaddockPackage, warn if we can't
    tryGetPackage pkgInfo = 
        (getPackage session pkgInfo >>= return . Just)
      `catchDyn`
        (\(e::HaddockException) -> do 
          let pkgName = showPackageId (package pkgInfo)
          putStrLn ("Warning: Cannot use package " ++ pkgName ++ ":")
          putStrLn ("   " ++ show e)
          return Nothing
        )


-- | Try to create a HaddockPackage structure for a package
getPackage :: Session -> InstalledPackageInfo -> IO HaddockPackage
getPackage session pkgInfo = do

  html <- getHtml pkgInfo
  ifacePath <- getIface pkgInfo
  iface <- readInterfaceFile ifacePath
  
  let docEnv  = ifDocEnv iface
      modules = packageModules pkgInfo

  return $ HaddockPackage {
    pdModules  = modules,
    pdDocEnv   = docEnv,
    pdHtmlPath = html
  } 


-- | Recreate exposed modules from an InstalledPackageInfo
packageModules :: InstalledPackageInfo -> [Module]
packageModules pkgInfo = map (mkModule (pkgId pkgInfo)) moduleNames
  where 
    moduleNames = map mkModuleName (exposedModules pkgInfo)
    pkgId = mkPackageId . package 


-- | Get the Haddock HTML directory path for a package
getHtml :: InstalledPackageInfo -> IO FilePath
getHtml pkgInfo = case haddockHTMLs pkgInfo of 
  (path:_) | not (null path) -> do
    dirExists <- doesDirectoryExist path
    if dirExists then return path else throwE $
       "HTML directory " ++ path ++ " does not exist."
  _ -> throwE "No Haddock documentation installed."


-- | Get the Haddock interface path for a package
getIface :: InstalledPackageInfo -> IO FilePath
getIface pkgInfo = case haddockInterfaces pkgInfo of
  (file:_) | not (null file) -> do
    fileExists <- doesFileExist file
    if fileExists then return file else throwE $
       "Interface file " ++ file ++ " does not exist."
  _ -> throwE "No Haddock interface installed."


-- | Build one big doc env out of a list of packages. If multiple packages 
-- export the same (original) name, we just pick one of the packages as the 
-- documentation site.
combineDocEnvs :: [HaddockPackage] -> DocEnv
combineDocEnvs packages = Map.unions (map pdDocEnv packages)

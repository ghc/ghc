--
-- Haddock - A Haskell Documentation Tool
--
-- (c) Simon Marlow 2003
--


module Haddock.Packages (
  HaddockPackage(..),
  getHaddockPackages,
  getHaddockPackages',
  combineLinkEnvs
) where


import Haddock.Types
import Haddock.Exception
import Haddock.InterfaceFile
import qualified Distribution.Haddock as D

import Data.Maybe
import qualified Data.Map as Map
import Control.Monad
import Control.Exception
import System.Directory

import GHC
import DynFlags
import Module
import Packages


-- | This structure represents the installed Haddock information for a 
-- package. This is basically the contents of the .haddock file, the path
-- to the html files and the list of modules in the package
data HaddockPackage = HaddockPackage {
  pdModules  :: [Module],
  pdLinkEnv  :: LinkEnv,
  pdHtmlPath :: FilePath
}


getHaddockPackages' :: [(FilePath, FilePath)] -> IO [HaddockPackage]
getHaddockPackages' pairs = do
  mbPackages <- mapM tryReadIface pairs
  return (catMaybes mbPackages)
  where
    -- try to get a HaddockPackage, warn if we can't
    tryReadIface (html, iface) = do
      eIface <- D.readInterfaceFile iface
      case eIface of
        Left err -> do
          putStrLn ("Warning: Cannot read " ++ iface ++ ":")
          putStrLn ("   " ++ show err)
          putStrLn "Skipping this interface."
          return Nothing
        Right iface -> return $ Just $
                       HaddockPackage (ifModules iface) (ifLinkEnv iface) html


-- | Try to read the installed Haddock information for the given packages, 
-- if it exists. Print a warning on stdout if it couldn't be found for a 
-- package.
getHaddockPackages :: [InstalledPackageInfo] -> IO [HaddockPackage]
getHaddockPackages pkgInfos = liftM catMaybes $ mapM tryGetPackage pkgInfos
  where
    -- try to get a HaddockPackage, warn if we can't
    tryGetPackage pkgInfo = 
        (getPackage pkgInfo >>= return . Just)
      `catchDyn`
        (\(e::HaddockException) -> do
          let pkgName = showPackageId (package pkgInfo)
          putStrLn ("Warning: Cannot use package " ++ pkgName ++ ":")
          putStrLn ("   " ++ show e)
          return Nothing
        )


-- | Try to read a HaddockPackage structure for a package
getPackage :: InstalledPackageInfo -> IO HaddockPackage
getPackage pkgInfo = do

  html      <- getHtml pkgInfo
  ifacePath <- getIface pkgInfo
  iface     <- readInterfaceFile ifacePath
  
  return $ HaddockPackage {
    pdModules  = ifModules iface,
    pdLinkEnv  = ifLinkEnv iface,
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


-- | Build one big link env out of a list of packages. If multiple packages 
-- export the same (original) name, we just pick one of the packages as the 
-- documentation site.
combineLinkEnvs :: [HaddockPackage] -> LinkEnv
combineLinkEnvs packages = Map.unions (map pdLinkEnv packages)

{-# LANGUAGE CPP, RecordWildCards, TypeSynonymInstances, StandaloneDeriving, GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
-- This module deliberately defines orphan instances for now. Should
-- become unnecessary once we move to using the binary package properly:
{-# OPTIONS_GHC -fno-warn-orphans #-}
#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Trustworthy #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.InstalledPackageInfo.Binary
-- Copyright   :  (c) The University of Glasgow 2009
--
-- Maintainer  :  cvs-ghc@haskell.org
-- Portability :  portable
--

module Distribution.InstalledPackageInfo.Binary (
       readBinPackageDB,
       writeBinPackageDB
  ) where

import Distribution.Version
import Distribution.Package hiding (depends)
import Distribution.License
import Distribution.InstalledPackageInfo as IPI
import Data.Binary as Bin
import Control.Exception as Exception

readBinPackageDB :: Binary m => FilePath -> IO [InstalledPackageInfo_ m]
readBinPackageDB file
    = do xs <- Bin.decodeFile file
         _ <- Exception.evaluate $ length xs
         return xs
      `catchUserError`
      (\err -> error ("While parsing " ++ show file ++ ": " ++ err))

catchUserError :: IO a -> (String -> IO a) -> IO a
catchUserError io f = io `Exception.catch` \(ErrorCall err) -> f err

writeBinPackageDB :: Binary m => FilePath -> [InstalledPackageInfo_ m] -> IO ()
writeBinPackageDB file ipis = Bin.encodeFile file ipis

instance Binary m => Binary (InstalledPackageInfo_ m) where
  put = putInstalledPackageInfo
  get = getInstalledPackageInfo

putInstalledPackageInfo :: Binary m => InstalledPackageInfo_ m -> Put
putInstalledPackageInfo ipi = do
  put (sourcePackageId ipi)
  put (installedPackageId ipi)
  put (license ipi)
  put (copyright ipi)
  put (maintainer ipi)
  put (author ipi)
  put (stability ipi)
  put (homepage ipi)
  put (pkgUrl ipi)
  put (synopsis ipi)
  put (description ipi)
  put (category ipi)
  put (exposed ipi)
  put (exposedModules ipi)
  put (hiddenModules ipi)
  put (trusted ipi)
  put (importDirs ipi)
  put (libraryDirs ipi)
  put (hsLibraries ipi)
  put (extraLibraries ipi)
  put (extraGHCiLibraries ipi)
  put (includeDirs ipi)
  put (includes ipi)
  put (IPI.depends ipi)
  put (hugsOptions ipi)
  put (ccOptions ipi)
  put (ldOptions ipi)
  put (frameworkDirs ipi)
  put (frameworks ipi)
  put (haddockInterfaces ipi)
  put (haddockHTMLs ipi)

getInstalledPackageInfo :: Binary m => Get (InstalledPackageInfo_ m)
getInstalledPackageInfo = do
  sourcePackageId <- get
  installedPackageId <- get
  license <- get
  copyright <- get
  maintainer <- get
  author <- get
  stability <- get
  homepage <- get
  pkgUrl <- get
  synopsis <- get
  description <- get
  category <- get
  exposed <- get
  exposedModules <- get
  hiddenModules <- get
  trusted <- get
  importDirs <- get
  libraryDirs <- get
  hsLibraries <- get
  extraLibraries <- get
  extraGHCiLibraries <- get
  includeDirs <- get
  includes <- get
  depends <- get
  hugsOptions <- get
  ccOptions <- get
  ldOptions <- get
  frameworkDirs <- get
  frameworks <- get
  haddockInterfaces <- get
  haddockHTMLs <- get
  return InstalledPackageInfo{..}

instance Binary PackageIdentifier where
  put pid = do put (pkgName pid); put (pkgVersion pid)
  get = do 
    pkgName <- get
    pkgVersion <- get
    return PackageIdentifier{..}

instance Binary License where
  put (GPL v)              = do putWord8 0; put v
  put (LGPL v)             = do putWord8 1; put v
  put BSD3                 = do putWord8 2
  put BSD4                 = do putWord8 3
  put MIT                  = do putWord8 4
  put PublicDomain         = do putWord8 5
  put AllRightsReserved    = do putWord8 6
  put OtherLicense         = do putWord8 7
  put (Apache v)           = do putWord8 8; put v
  put (UnknownLicense str) = do putWord8 9; put str

  get = do
    n <- getWord8
    case n of
      0 -> do v <- get; return (GPL v)
      1 -> do v <- get; return (LGPL v)
      2 -> return BSD3
      3 -> return BSD4
      4 -> return MIT
      5 -> return PublicDomain
      6 -> return AllRightsReserved
      7 -> return OtherLicense
      8 -> do v <- get; return (Apache v)
      _ -> do str <- get; return (UnknownLicense str)

instance Binary Version where
  put v = do put (versionBranch v); put (versionTags v)
  get = do versionBranch <- get; versionTags <- get; return Version{..}

deriving instance Binary PackageName
deriving instance Binary InstalledPackageId

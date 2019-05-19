module Rules.Download (downloadRules) where

import Hadrian.BuildPath
import qualified Text.Parsec      as Parsec

import Base
import Context
import Expression hiding (way, package)
import Packages
import Target
import Utilities

-- * Rules for downloading a package from an external source

downloadRules :: Rules ()
downloadRules = do
    root <- buildRootRules
    root -/- downloadedDir -/- "//*.cabal"  %> downloadLibrary

-- | Parses root -/- downloadedDir -/- pkgname-version -/-
parsePackage :: FilePath -> Parsec.Parsec String () String
parsePackage root = do
  void $ Parsec.string root
  void $ Parsec.char '/'
  void $ Parsec.string downloadedDir
  void $ Parsec.char '/'
  Parsec.manyTill Parsec.anyChar (Parsec.try $ Parsec.string "/")

-- | Download a library using `cabal get`
downloadLibrary :: FilePath -> Action ()
downloadLibrary fp = do
  root <- buildRoot
  p <- parsePath (parsePackage  root) "package name" fp
  dPath <- downloadedPath
  let ctx = Context Stage0 ghc vanilla
  build $ target ctx SysCabalGet [p] []
  copyDirectory ("/tmp" </> p) dPath
  removeDirectory ("/tmp" </> p)

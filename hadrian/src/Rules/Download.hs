module Rules.Download (downloadRules) where

import Hadrian.BuildPath
import Hadrian.Haskell.Cabal
import Hadrian.Haskell.Cabal.Type
import qualified Text.Parsec      as Parsec

import Base
import Context
import Expression hiding (way, package)
import Oracles.ModuleFiles
import Packages
import Rules.Gmp
import Rules.Libffi (libffiDependencies)
import Target
import Utilities
import Debug.Trace

-- * Library 'Rules'

downloadRules :: Rules ()
downloadRules = do
    root <- buildRootRules
    root -/- downloadedDir -/- "//*.cabal"  %> downloadLibrary

parsePackage :: FilePath -> Parsec.Parsec String () String
parsePackage root = do
  Parsec.string root
  Parsec.char '/'
  Parsec.string downloadedDir
  Parsec.char '/'
  Parsec.manyTill Parsec.anyChar (Parsec.try $ Parsec.string "/")



downloadLibrary :: FilePath -> Action ()
downloadLibrary fp = do
  traceShowM fp
  root <- buildRoot
  p <- parsePath (parsePackage  root) "package name" fp
  traceShowM p
  dPath <- downloadedPath
  let ctx = Context Stage0 ghc vanilla
  build $ target ctx (SysCabalGet) [p] []
  copyDirectory ("/tmp" </> p) dPath
  removeDirectory ("/tmp" </> p)

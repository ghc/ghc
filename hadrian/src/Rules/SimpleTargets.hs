module Rules.SimpleTargets (simplePackageTargets, simpleTargetString) where

import Base
import Context
import Packages
import Settings

import Data.Foldable

import Base
import Context
import Hadrian.BuildPath
import Hadrian.Expression
import Hadrian.Haskell.Cabal
import Oracles.Setting
import Packages
import Rules.Gmp
import Rules.Rts
import Settings
import Target
import Utilities
import Rules.Library
import Hadrian.Oracles.Cabal
import Hadrian.Haskell.Cabal.Type
import Hadrian.Haskell.Cabal.Parse

import Distribution.Version (Version)
import qualified Distribution.Parsec as Cabal
import qualified Distribution.Types.PackageName as Cabal
import qualified Distribution.Types.PackageId as Cabal
import qualified Distribution.Version as Cabal


-- | Simple aliases for library and executable targets.
--
--  - @stage<N>:lib:<name>@ will build library @name@ with
--    the stage N compiler, putting the result under
--    @<build root>/stage<N>/lib@.
--  - @stage<N>:exe:<name>@ will build executable @name@
--    with the stage N-1 compiler, putting the result under
--    @<build root>/stage<N-1>/bin.
simplePackageTargets :: [(Resource, Int)] -> Rules ()
simplePackageTargets rs = traverse_ (simpleTarget rs) targets

  where targets = [ (stage, target, boot)
                  | stage <- [minBound..maxBound]
                  , target <- knownPackages
                  , boot <- if isLibrary target then [False, True] else [False]
                  ]




simpleTarget :: [(Resource, Int)] -> (Stage, Package, Bool) -> Rules ()
simpleTarget rs (stage, target, boot) = do
  let tgt = simpleTargetString boot stage target
  tgt ~> do
    getTargetPath rs boot stage target


getTargetPath :: [(Resource, Int)] -> Bool -> Stage -> Package -> Action ()
getTargetPath rs boot stage pkg
  | isLibrary pkg = libraryRule rs boot stage pkg
  | otherwise     = getProgramPath stage pkg >>= \p -> need [p]

getLibraryPath :: Stage -> Package -> Action FilePath
getLibraryPath stage pkg = pkgConfFile (vanillaContext stage pkg)

getProgramPath :: Stage -> Package -> Action FilePath
getProgramPath Stage0 _ =
  error ("Cannot build a stage 0 executable target: " ++
         "it is the boot compiler's toolchain")
getProgramPath stage pkg = programPath (vanillaContext (pred stage) pkg)

libraryRule rs boot stage pkg = do
  conf <- getLibraryPath stage pkg
--  produces [conf]
  liftIO $ print ("Needing", conf)
  historyDisable
  let libpath = takeDirectory (takeDirectory conf)
      settings = libpath -/- "settings"
      platformConstants = libpath -/- "platformConstants"

  --need [settings, platformConstants]

  let ctx = Context stage pkg vanilla
  if boot
     then copyConf rs ctx conf
     else do
        pd <- readPackageData pkg
        buildConf rs ctx conf



module Hadrian.Haskell.Cabal.Parse where

import Context.Type (Context)
import Development.Shake (Action)
import Hadrian.Haskell.Cabal.PackageData (PackageData)
import Hadrian.Haskell.Cabal.Type (Cabal)

parseCabal :: Context -> Action Cabal
parsePackageData :: Context -> Action PackageData

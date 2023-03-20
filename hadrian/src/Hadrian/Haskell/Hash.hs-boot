module Hadrian.Haskell.Hash where

import Context.Type
import Hadrian.Package
import Development.Shake

pkgUnitId :: Context -> Package -> Action String


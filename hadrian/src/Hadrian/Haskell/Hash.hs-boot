module Hadrian.Haskell.Hash where

import Hadrian.Package
import Stage
import Development.Shake

pkgUnitId :: Stage -> Package -> Action String


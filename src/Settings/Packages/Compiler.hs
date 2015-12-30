module Settings.Packages.Compiler (compilerPackageArgs) where

import Base
import Expression
import GHC (compiler)
import Predicates (builder, builderGhc, package)

compilerPackageArgs :: Args
compilerPackageArgs = package compiler ? do
    stage <- getStage
    mconcat [ builder Alex ? arg "--latin1"

            , builderGhc ? arg ("-I" ++ pkgPath compiler -/- stageString stage) ]

module Settings.Packages.Compiler (compilerArgs) where

import Expression
import GHC (compiler)
import Predicates (builder, package)

compilerArgs :: Args
compilerArgs = package compiler ?
    mconcat [ builder Alex ? arg "--latin1" ]

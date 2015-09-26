module Settings.Builders.Alex (alexArgs) where

import Expression
import GHC (compiler)
import Predicates (builder, package)

alexArgs :: Args
alexArgs = builder Alex ? mconcat [ arg "-g"
                                  , package compiler ? arg "--latin1"
                                  , arg =<< getInput
                                  , arg "-o", arg =<< getOutput ]

-- TODO: separate arguments into builder-specific and package-specific
-- compilierArgs = package compiler ? builder Alex ? arg "awe"

-- args = mconcat
--     [ alexArgs
--     , compilerArgs ]

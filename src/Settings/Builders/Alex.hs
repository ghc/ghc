module Settings.Builders.Alex (alexArgs) where

import Expression
import GHC (compiler)
import Predicates (builder, package)

alexArgs :: Args
alexArgs = builder Alex ? do
    src  <- getSource
    file <- getFile
    mconcat [ arg "-g"
            , package compiler ? arg "--latin1"
            , arg src
            , arg "-o", arg file ]

-- TODO:
-- compilierArgs = package compiler ? builder Alex ? arg "awe"

-- args = mconcat
--     [ alexArgs
--     , compilerArgs ]

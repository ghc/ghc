module Rules.Util (
    build
    ) where

import Base
import Util
import Settings
import Expression
import Oracles.Builder
import Oracles.ArgsHash

build :: FullTarget -> Action ()
build target = do
    argList <- interpret target args
    putColoured Green (show target)
    putColoured Green (show argList)
    -- The line below forces the rule to be rerun if the args hash has changed
    argsHash <- askArgsHash target
    run (getBuilder target) argList

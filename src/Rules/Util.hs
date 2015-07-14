module Rules.Util (
    build
    ) where

import Base
import Util
import Settings
import Expression
import Oracles.Builder
import Oracles.ArgsHash

build :: Target -> Action ()
build target = do
    args <- interpret target settings
    putColoured Green (show target)
    putColoured Green (show args)
    -- The line below forces the rule to be rerun if the args hash has changed
    argsHash <- askArgsHash target
    run (getBuilder target) args

{-# LANGUAGE Arrows #-}
module NoBlockArgumentsFailArrowCmds where

import Control.Arrow

cmdLam :: () -> ()
cmdLam = proc () -> (| id \() -> () >- returnA |) ()

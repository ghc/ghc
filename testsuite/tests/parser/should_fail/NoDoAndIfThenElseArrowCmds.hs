{-# LANGUAGE NoDoAndIfThenElse #-}
{-# LANGUAGE Arrows #-}

module NoDoAndIfThenElseArrowCmds where

import Control.Arrow

foo :: () -> ()
foo = proc () -> do if True
                    then () >- returnA
                    else () >- returnA

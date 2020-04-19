{-# LANGUAGE Arrows, BlockArguments #-}

module BlockArgumentsArrowCmds where

import Control.Arrow

cmdLam :: () -> ()
cmdLam = proc () -> (| id \() -> () >- returnA |) ()

cmdCase :: () -> ()
cmdCase = proc () -> (| id case () of
  () -> () >- returnA |)

cmdIf :: () -> ()
cmdIf = proc () -> (| id if True then () >- returnA else () >- returnA |)

cmdLet :: () -> ()
cmdLet = proc () -> (| id let x = () in x >- returnA |)

cmdDo :: () -> ()
cmdDo = proc () -> (| id do
  () >- returnA |)

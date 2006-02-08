{-# OPTIONS -fglasgow-exts -O #-}

-- This one showed up a bug that required type refinement in TcIface
-- See the call to coreRefineTys in TcIface
--
-- Tests for bug: http://hackage.haskell.org/trac/ghc/ticket/685

module ShouldCompile where

import Gadt17_help ( TernOp (..), applyTernOp )

liftTernOpObs :: TernOp a b c d -> a -> b -> c ->  d
liftTernOpObs op x y z = applyTernOp op x y z

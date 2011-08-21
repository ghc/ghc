{-# LANGUAGE ImplicitParams, MagicHash #-}

module ShouldFail where

import GHC.Exts

-- As of GHC 7.4, we don't allow unlifted types in ImplicitParms
foo :: (?x :: Int#) => Int
foo = I# ?x

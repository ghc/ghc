{-# LANGUAGE ExperimentalDependentTypes #-}
module TypeLetPrint where

type F a = let b {- bind -} = G a
           in {- reuse -} H b b

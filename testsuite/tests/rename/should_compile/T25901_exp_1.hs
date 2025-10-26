{-# LANGUAGE ExplicitNamespaces #-}

module T25901_exp_1 where

import T25901_exp_1_helper (type C, type D, type T, type K, type K1)

g :: C a => D a -> T -> p (K1 :: K) -> ()
g _ _ _ = ()
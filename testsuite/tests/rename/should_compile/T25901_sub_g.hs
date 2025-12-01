{-# LANGUAGE ExplicitNamespaces #-}

module T25901_sub_g where

import T25901_sub_g_helper qualified as T1 (T (data ..)) -- T and MkT
import T25901_sub_g_helper qualified as T2 (T (type ..)) -- T only
import T25901_sub_g_helper qualified as T3 (type T (..)) -- T and MkT

t1 :: T1.T
t1 = T1.MkT

t2 :: T2.T
t2 = t2

t3 :: T3.T
t3 = T3.MkT
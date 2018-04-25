{-# LANGUAGE
  MonoLocalBinds,
  PolyKinds,

  FunctionalDependencies,
  FlexibleContexts #-}

-- reduced version of
-- http://code.haskell.org/~aavogt/HList_dredge_ghc802/Data/HList/Dredge.hs
--
-- F ~ EnsureLabel
-- g_f  ~ dredge
-- g_f' ~ dredge'

{- ghc-8.0.2

too_eager.hs:25:14: warning: [-Wdeferred-type-errors]
    • No instance for (F t0 (Proxy b0)) arising from a use of ‘g_f’
    • In the first argument of ‘id’, namely ‘(g_f a)’
      In the expression: id (g_f a)
      In an equation for ‘g_f'’: g_f' a = id (g_f a)


ghc-7.10.3 can wait to select the instance for F

-}

module T13371 where

import Data.Proxy

class F a b | a -> b where
  f :: a -> b

g ::  Proxy b -> c
g _ = undefined

-- a type signature (what ghc infers) makes it work
-- g_f :: F a (Proxy b) => a -> c
g_f a = g (f a)

g_f' a = id (g_f a)

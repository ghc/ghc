-- Same as T13233, but we compile with -fprint-typechecker-elaboration.
-- The error message should show an eta-expanded call to (#,#).
-- See comments at https://gitlab.haskell.org/ghc/ghc/merge_requests/2540
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MagicHash #-}
module Bug where

import GHC.Exts (TYPE, RuntimeRep, Weak#, State#, RealWorld, mkWeak# )

class Foo (a :: TYPE rep) where
  bar :: forall rep2 (b :: TYPE rep2). (a -> a -> b) -> a -> a -> b

baz :: forall rep (a :: TYPE rep). Foo a => a -> a -> (# a, a #)
baz = bar (#,#)

obscure :: (forall (rep1 :: RuntimeRep) (rep2 :: RuntimeRep)
                   (a :: TYPE rep1) (b :: TYPE rep2).
                   a -> b -> (# a, b #)) -> ()
obscure _ = ()

quux :: ()
quux = obscure (#,#)

-- It used to be that primops has no binding. However, as described in
-- Note [Primop wrappers] in GHC.Builtin.PrimOps we now rewrite unsaturated primop
-- applications to their wrapper, which allows safe use of levity polymorphism.
primop :: forall (rep :: RuntimeRep) (a :: TYPE rep) b c.
          a -> b -> (State# RealWorld -> (# State# RealWorld, c #))
       -> State# RealWorld -> (# State# RealWorld, Weak# b #)
primop = mkWeak#

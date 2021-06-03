{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MagicHash #-}
module Bug where

import GHC.Exts ( TYPE, RuntimeRep(BoxedRep), Levity
                , Weak#, State#, RealWorld, mkWeak#
                )

primop :: forall (l :: Levity) (a :: TYPE ('BoxedRep l)) b c.
          a -> b -> (State# RealWorld -> (# State# RealWorld, c #))
       -> State# RealWorld -> (# State# RealWorld, Weak# b #)
primop = mkWeak#

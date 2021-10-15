{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MagicHash #-}

module RepPolyWrappedVar where

import GHC.Exts

primop :: forall (l :: Levity) (a :: TYPE ('BoxedRep l)).
          a -> Int -> (State# RealWorld -> (# State# RealWorld, Bool #))
       -> State# RealWorld -> (# State# RealWorld, Weak# Int #)
primop = mkWeak# @a @Int @Bool

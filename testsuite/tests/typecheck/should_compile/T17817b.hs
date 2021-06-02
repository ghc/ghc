{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MagicHash #-}
module Bug where

import GHC.Exts ( Weak#, State#, RealWorld, mkWeak# )
import GHC.Types ( UnliftedType )

primop1 :: forall a b c.
          a -> b -> (State# RealWorld -> (# State# RealWorld, c #))
       -> State# RealWorld -> (# State# RealWorld, Weak# b #)
primop1 = mkWeak#

primop2 :: forall (a :: UnliftedType) b c.
          a -> b -> (State# RealWorld -> (# State# RealWorld, c #))
       -> State# RealWorld -> (# State# RealWorld, Weak# b #)
primop2 = mkWeak#

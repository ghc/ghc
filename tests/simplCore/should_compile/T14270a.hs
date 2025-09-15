{-# LANGUAGE TypeApplications, ScopedTypeVariables, GADTs, RankNTypes,
             PolyKinds, KindSignatures #-}
{-# OPTIONS_GHC -O2 #-} -- We are provoking a bug in GHC.Core.Opt.SpecConstr

module T14270a where

import Data.Kind
import Data.Proxy

data T a = T1 (T a) | T2

type K :: k -> Type
data K a where
  K1 :: K (a :: Type)
  K2 :: K a

f :: T (a :: Type) -> Bool
f (T1 x) = f x
f T2     = True

g :: forall k (a :: k). K a -> T a -> Bool
g kv x = case kv of
            K1 -> f @a T2   -- f @a (T1 x) gives a different crash
            k2 -> True

-- The point here is that the call to f looks like
--    f @(a |> co) (T2 @(a |> co))
-- where 'co' is bound by the pattern match on K1
-- See Note [SpecConstr and casts] in GHC.Core.Opt.SpecConstr

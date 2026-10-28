{-# LANGUAGE DataKinds, TypeFamilies, PolyKinds, GADTs, AllowAmbiguousTypes #-}
module T27994 where

import Data.Kind (Type)
import Data.Proxy (Proxy)

type F :: forall{k}. k -> Type -> Type
type family F s x where
   F (s :: Type -> k) (Proxy s) = Int

f :: forall k (s :: k) x. F s x -> x
f _ = undefined

g :: Proxy Maybe -> ()
g _ = ()

data T a where
  MkT :: T Int

test :: T a -> ()
test t = (\y -> case t of MkT -> g y) (f (3 :: Int))

{- [W] F @k_azX (s_azY::k_azX) p_azN ~ Int

unify   [*->k,      s::*->k,  Proxy @(*->k) (s::*->k), Int]
          ~ [k_azX, s_azY::k_azX, p_azN,                   Int]

  -->   [ k_azX :-> (* -> k)
        , s     :-> s_azY::k_azX
        , p_azN :-> Proxy @(*->k) (s::*->k) ]

  fixpt -->   [ k_azX :-> Type -> k
              , s     :-> s_azY::(Type -> k)
              , p_azN :-> Proxy @(Type->k) (s::k_azX) ]

Generate a [W] for each pair
    [W] k ~ k_azX
    [W] s ~ s_azY
    [W] Proxy s ~ p_azN
    [W] Int ~ Int
As an opt (claim) apply subst first
    [W] k_azX ~ k_azX
    [W] s_azY ~ s_azY
    [W] Proxy s_azY ~ p_azN
    [W] Int ~ Int

-- [W] F @k_azX s_azY[tau] p_azN ~ Int
-- Want: fresh k' s'
-- [W] k' ~ k_azX
-- [W] s':k' ~ s_azY:k_azX
-- [W] Proxy s' ~ x
-}


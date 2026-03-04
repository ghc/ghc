{- This program used to cause a panic (#15588), but it should possibly even be
accepted. See #15589. -}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
module T15588 where

import Data.Proxy
import Data.Type.Equality
import Data.Type.Bool
import Data.Kind

data SameKind :: forall k. k -> k -> Type
type family IfK (e :: Proxy (j :: Bool)) (f :: m) (g :: n) :: If j m n where
   IfK (_ :: Proxy True)  f _ = f
   IfK (_ :: Proxy False) _ g = g

y :: forall ck (c :: ck). ck :~: Proxy True -> ()
y Refl = let x :: forall a b (d :: a). SameKind (IfK c b d) d
             x = undefined
         in ()


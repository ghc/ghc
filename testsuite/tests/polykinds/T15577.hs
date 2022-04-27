{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Bug where

import Data.Kind
import Data.Proxy
import Data.Type.Equality

type family F (x :: f (a :: k)) :: f a

f :: forall k (f :: k -> Type) (a :: k) (r :: f a). Proxy r -> F r :~: r
f = undefined

g :: forall (f :: Type -> Type) (a :: Type) (r :: f a). Proxy r -> F r :~: r
g r | Refl <- f -- Uncommenting the line below makes it work again
                -- @Type
                @f @a @r r
    = Refl

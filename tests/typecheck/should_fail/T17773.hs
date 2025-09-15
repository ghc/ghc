{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Bug where

import Data.Proxy (Proxy(..))
import Data.Type.Equality ((:~:)(..))

type family (x :: f a) <|> (y :: f a) :: f a
type family Mzero :: f a

monadPlusMplus :: forall f a (x :: f a) (y :: f a).
                  Proxy x -> Proxy y
               -> Mzero x y :~: (x <|> y)
monadPlusMplus _ _ = _Refl

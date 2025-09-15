{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

module PatternsBug where

import Data.Kind
  ( Type )
import Data.Proxy
  ( Proxy(..) )
import Data.Type.Equality
  ( (:~:)(Refl) )
import Unsafe.Coerce
  ( unsafeCoerce )

passGetterIndex
  :: forall (js :: [Type]) (jss :: [[Type]]) (s :: Type) (as :: [Type])
  . SSameLength js as -> HList js -> Viewers (js ': jss) s as -> Viewers jss s as
passGetterIndex (SSameSucc ( same1 :: SSameLength t_js t_as ) ) js views =
  case ( js, views ) of
    ( (k :: j) :> (ks :: HList t_js), ConsViewer same2 (_ :: Proxy is) (_ :: Proxy iss) _ getters ) ->
      case same2 of
        ( sameSucc@(SSameSucc (same3 :: SSameLength t_is jss) ) ) ->
          case sameSucc of
            ( _ :: SSameLength (i ': t_is) (js ': jss) )
              | Refl <- ( unsafeCoerce Refl :: ZipCons t_is (Tail iss) :~: jss )
              , Refl <- ( unsafeCoerce Refl :: ( t_js ': MapTail jss ) :~: iss )
              , Refl <- ( unsafeCoerce Refl :: i :~: j )
          --    , Proxy :: Proxy bss <- Proxy @(Tail iss)
              -> ConsViewer same3 (Proxy @t_is) (Proxy @(Tail iss)) Proxy
          --      -> ConsViewer same3 (Proxy @t_is) (Proxy @bss) Proxy
                ( passGetterIndex @t_js @(MapTail jss) @s @t_as same1 ks getters )

data Viewers (iss :: [[Type]]) (s :: Type) (as :: [Type]) where
  -- NilViewer ...
  ConsViewer :: forall (is :: [Type]) (s :: Type) (a :: Type) (iss :: [[Type]]) (as :: [Type])
             .  SSameLength is (ZipCons is iss)
             -> Proxy is
             -> Proxy iss
             -> Proxy a
             -> Viewers iss s as
             -> Viewers (ZipCons is iss) s (a ': as)

data SSameLength (is :: [k]) (js :: [l]) where
  SSameZero :: SSameLength '[] '[]
  SSameSucc :: SSameLength is js -> SSameLength (i ': is) (j ': js)

infixr 3 :>
data HList (as :: [Type]) where
  HNil :: HList '[]
  (:>) :: a -> HList as -> HList (a ': as)

type family ListVariadic (as :: [Type]) (b :: Type) = (r :: Type) | r -> as b where

type family ZipCons (as :: [k]) (bss :: [[k]]) = (r :: [[k]]) | r -> as bss where
  ZipCons '[] '[] = '[]
  ZipCons (a ': as) (bs ': bss) = (a ': bs) ': ZipCons as bss

type family Tail (x :: [k]) :: [k] where
  Tail (_ ': xs) = xs

type family MapTail (x :: [[k]]) :: [[k]] where
  MapTail '[]         = '[]
  MapTail (xs ': xss) = Tail xs ': MapTail xss

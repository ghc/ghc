{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
module T14720 where

import Data.Kind (Type)
import Data.Type.Equality ((:~:)(..), sym, trans)
import Data.Void

data family Sing (z :: k)

class Generic (a :: Type) where
    type Rep a :: Type
    from :: a -> Rep a
    to :: Rep a -> a

class PGeneric (a :: Type) where
  type PFrom (x :: a)     :: Rep a
  type PTo   (x :: Rep a) :: a

class SGeneric k where
  sFrom :: forall (a :: k).     Sing a -> Sing (PFrom a)
  sTo   :: forall (a :: Rep k). Sing a -> Sing (PTo a :: k)

class (PGeneric k, SGeneric k) => VGeneric k where
  sTof  :: forall (a :: k).     Sing a -> PTo (PFrom a) :~: a
  sFot  :: forall (a :: Rep k). Sing a -> PFrom (PTo a :: k) :~: a

data Decision a = Proved a
                | Disproved (a -> Void)

class SDecide k where
  (%~) :: forall (a :: k) (b :: k). Sing a -> Sing b -> Decision (a :~: b)
  default (%~) :: forall (a :: k) (b :: k). (VGeneric k, SDecide (Rep k))
               => Sing a -> Sing b -> Decision (a :~: b)
  s1 %~ s2 = case sFrom s1 %~ sFrom s2 of
    Proved (Refl :: PFrom a :~: PFrom b) ->
      case (sTof s1, sTof s2) of
          (Refl, Refl) -> Proved Refl
    Disproved contra -> Disproved (\Refl -> contra Refl)

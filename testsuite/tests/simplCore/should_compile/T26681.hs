{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module T26681 where

import Data.Kind (Type)
import Data.Type.Equality
import GHC.TypeLits
import qualified Unsafe.Coerce


{-# NOINLINE unsafeCoerceRefl #-}
unsafeCoerceRefl :: a :~: b
unsafeCoerceRefl = Unsafe.Coerce.unsafeCoerce Refl

type family MapJust l where
  MapJust '[] = '[]
  MapJust (x : xs) = Just x : MapJust xs

type family Tail l where
  Tail (_ : xs) = xs

lemMapJustCons :: MapJust sh :~: Just n : sh' -> sh :~: n : Tail sh
lemMapJustCons Refl = unsafeCoerceRefl


type ListX :: [Maybe Nat] -> (Maybe Nat -> Type) -> Type
data ListX sh f where
  ConsX :: !(f n) -> ListX (n : sh) f


data JustN n where
  JustN :: JustN (Just n)

data UnconsListSRes f sh1 = forall n sh. (n : sh ~ sh1) => UnconsListSRes

listsUncons :: forall sh1 f. ListX (MapJust sh1) JustN -> UnconsListSRes f sh1
listsUncons (ConsX JustN)
  | Refl <- lemMapJustCons @sh1 Refl
  = UnconsListSRes

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}

module T6137 where

data Sum a b = L a | R b

data Sum1 (a :: k1 -> *) (b :: k2 -> *) :: Sum k1 k2 -> * where
  LL :: a i -> Sum1 a b (L i)
  RR :: b i -> Sum1 a b (R i)

data Code i o = F (Code (Sum i o) o)

-- An interpretation for `Code` using a data family works:
data family In (f :: Code i o) :: (i -> *) -> (o -> *)

data instance In (F f) r o where
  MkIn :: In f (Sum1 r (In (F f) r)) o -> In (F f) r o

-- Requires polymorphic recursion
data In' (f :: Code i o) :: (i -> *) -> o -> * where
  MkIn' :: In' g (Sum1 r (In' (F g) r)) t -> In' (F g) r t

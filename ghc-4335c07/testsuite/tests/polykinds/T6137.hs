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

data instance In (F f) r x where
  MkIn :: In f (Sum1 r (In (F f) r)) x -> In (F f) r x


{- data R:InioFrx o i f r x where
     where  MkIn :: forall o i (f :: Code (Sum i o) o) (r :: i -> *) (x :: o).
                    In (Sum i o) o f (Sum1 o i r (In i o ('F i o f) r)) x
                    -> R:InioFrx o i f r x

   So  R:InioFrx :: forall o i. Code i o -> (i -> *) -> o -> *

  data family In i o (f :: Code i o) (a :: i -> *) (b :: o)

  axiom D:R:InioFrx0 ::
    forall o i (f :: Code (Sum i o) o).
      In i o ('F i o f) = R:InioFrx o i f


  D:R:InioFrx0 ::    R:InioFrx o i f ~ In i o ('F i o f)
-}
-- Requires polymorphic recursion
data In' (f :: Code i o) :: (i -> *) -> o -> * where
  MkIn' :: In' g (Sum1 r (In' (F g) r)) t -> In' (F g) r t

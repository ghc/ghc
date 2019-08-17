{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}

module T11362 where
-- this file when compiled with -dunique-increment=-1 made GHC crash

import Data.Kind (Type)

data Sum a b = L a | R b

data Sum1 (a :: k1 -> Type) (b :: k2 -> Type) :: Sum k1 k2 -> Type where
  LL :: a i -> Sum1 a b (L i)
  RR :: b i -> Sum1 a b (R i)

data Code i o = F (Code (Sum i o) o)

-- An interpretation for `Code` using a data family works:
data family In (f :: Code i o) :: (i -> Type) -> (o -> Type)

data instance In (F f) r o where
  MkIn :: In f (Sum1 r (In (F f) r)) o -> In (F f) r o

-- Requires polymorphic recursion
data In' (f :: Code i o) :: (i -> Type) -> o -> Type where
  MkIn' :: In' g (Sum1 r (In' (F g) r)) t -> In' (F g) r t

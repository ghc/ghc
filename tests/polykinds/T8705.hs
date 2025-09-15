{-# LANGUAGE TypeOperators, DataKinds, PolyKinds,
             MultiParamTypeClasses, GADTs, ConstraintKinds, TypeFamilies #-}
module T8705 where

data family Sing (a :: k)
data Proxy a = Proxy

data instance Sing (a :: Maybe k) where
  SJust :: Sing h -> Sing (Just h)

data Dict c where
  Dict :: c => Dict c

-- A less-than-or-equal relation among naturals
class a :<=: b

sLeq :: Sing n -> Sing n2 -> Dict (n :<=: n2)
sLeq = undefined

insert_ascending :: (lst ~ Just n1) => Proxy n1 -> Sing n -> Sing lst -> Dict (n :<=: n1)
insert_ascending _ n (SJust h)
  = case sLeq n h of
      Dict -> Dict

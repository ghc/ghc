{-# LANGUAGE TypeFamilies, DataKinds, TypeOperators, GADTs #-}
module T7967 where

data Nat = Zero | Succ Nat

data SNat :: Nat -> * where
  SZero :: SNat Zero
  SSucc :: SNat n -> SNat (Succ n)

data HList :: [*] -> * where
  HNil :: HList '[]
  HCons :: h -> HList t -> HList (h ': t)

data Index :: Nat -> [*] -> * where
  IZero :: Index Zero (h ': t)
  ISucc :: Index n l -> Index (Succ n) (h ': l)

type family Lookup (n :: Nat) (l :: [*]) :: *
type instance Lookup Zero (h ': t) = h
type instance Lookup (Succ n) (h ': t) = Lookup n t

hLookup :: Index n l -> HList l -> Lookup n l
hLookup IZero (HCons h _) = h
hLookup (ISucc n) (HCons _ t) = hLookup n t
hLookup _ _ = undefined

-- So far, so good. But, I wanted a way to convert `SNat`s to `Index`es. When
-- I add the (wrong) code below, and got a bogus error above

sNatToIndex :: SNat n -> HList l -> Index n l
sNatToIndex SZero HNil = IZero

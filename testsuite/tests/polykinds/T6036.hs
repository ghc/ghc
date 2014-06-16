{-# LANGUAGE DataKinds, TypeFamilies, PolyKinds, GADTs #-}

module T6036 where

data family Sing (a :: k)

data instance Sing (a :: Maybe k) where
  SNothing :: Sing 'Nothing
  SJust :: Sing b -> Sing ('Just b)

data Nat = Zero | Succ Nat

data instance Sing (a :: Nat) where
  SZero :: Sing Zero
  SSucc :: Sing n -> Sing (Succ n)

term = SJust SZero

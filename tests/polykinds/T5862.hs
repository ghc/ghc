{-# LANGUAGE PolyKinds,
             TypeFamilies,
             GADTs,
             DataKinds,
             KindSignatures
 #-}

module T5862 where

import Data.Kind (Type)

data Nat = Zero | Succ Nat

data SNat a where
  SZero :: SNat 'Zero
  SSucc :: SNat n -> SNat ('Succ n)

data SBool a where
  SFalse :: SBool 'False
  STrue :: SBool 'True

data SMaybe a where
  SNothing :: SMaybe 'Nothing
  SJust :: Sing a -> SMaybe ('Just a)

type family Sing (a :: k)
type instance Sing (a :: Nat) = SNat a
type instance Sing (a :: Bool) = SBool a
type instance Sing (a :: Maybe Type) = SMaybe a -- want to say Maybe k

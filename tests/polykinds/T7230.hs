{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE DataKinds, PolyKinds, GADTs, TypeFamilies #-}
{-# LANGUAGE TypeOperators, UndecidableInstances #-}
module T7230 where

data Nat = Zero | Succ Nat deriving (Show, Eq, Ord)

data family Sing (x :: k)

data instance Sing (n :: Nat) where
  SZero :: Sing Zero
  SSucc :: Sing n -> Sing (Succ n)

type SNat (n :: Nat) = Sing n

data instance Sing (b :: Bool) where
  STrue  :: Sing True
  SFalse :: Sing False

type SBool (b :: Bool) = Sing b

data instance Sing (xs :: [k]) where
  SNil  :: Sing ('[] :: [k])
  SCons :: Sing x -> Sing xs -> Sing (x ': xs)

type SList (xs :: [k]) = Sing (xs :: [k])

type family (:<<=) (n :: Nat) (m :: Nat) :: Bool
type instance Zero   :<<= n      = True
type instance Succ n :<<= Zero   = False
type instance Succ n :<<= Succ m = n :<<= m

(%:<<=) :: SNat n -> SNat m -> SBool (n :<<= m)
SZero   %:<<= _       = STrue
SSucc _ %:<<= SZero   = SFalse
SSucc n %:<<= SSucc m = n %:<<= m

type family   (b :: Bool) :&& (b' :: Bool) :: Bool
type instance True  :&& b = b
type instance False :&& b = False

type family   Increasing (xs :: [Nat]) :: Bool
type instance Increasing '[]  = True
type instance Increasing '[n] = True
type instance Increasing (n ': m ': ns) = n :<<= m :&& Increasing (m ': ns)

crash :: (Increasing xs) ~ True => SList xs -> SBool (Increasing xs)
crash (SCons x (SCons y xs)) = x %:<<= y
crash _                      = STrue

{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE FunctionalDependencies  #-}
{-# LANGUAGE GADTs                   #-}
{-# LANGUAGE LambdaCase              #-}
{-# LANGUAGE PatternSynonyms         #-}
{-# LANGUAGE PolyKinds               #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE TypeOperators           #-}
module T15517a () where

import Data.Kind (Type)
import Data.Proxy

newtype Rep (ki :: kon -> Type) (phi :: Nat -> Type) (code :: [[Atom kon]])
  = Rep (NS (PoA ki phi) code)

data NA  :: (kon -> Type) -> (Nat -> Type) -> Atom kon -> Type where
  NA_I :: (IsNat k) => phi k -> NA ki phi (I k)
  NA_K ::              ki  k -> NA ki phi (K k)

data NP :: (k -> Type) -> [k] -> Type where
  NP0  :: NP p '[]
  (:*) :: p x -> NP p xs -> NP p (x : xs)

class IsNat (n :: Nat) where
  getSNat :: Proxy n -> SNat n
instance IsNat Z where
  getSNat _ = SZ
instance IsNat n => IsNat (S n) where
  getSNat p = SS (getSNat $ proxyUnsuc p)

proxyUnsuc :: Proxy (S n) -> Proxy n
proxyUnsuc _ = Proxy

type PoA (ki :: kon -> Type) (phi :: Nat -> Type) = NP (NA ki phi)

data Atom kon
  = K kon
  | I Nat

data Nat = S Nat | Z
data SNat :: Nat -> Type where
  SZ ::           SNat Z
  SS :: SNat n -> SNat (S n)

data Kon = KInt
data Singl (kon :: Kon) :: Type where
  SInt :: Int -> Singl KInt

type family Lkup (n :: Nat) (ks :: [k]) :: k where
  Lkup Z     (k : ks) = k
  Lkup (S n) (k : ks) = Lkup n ks

data El :: [Type] -> Nat -> Type where
  El :: IsNat ix => Lkup ix fam -> El fam ix

data NS :: (k -> Type) -> [k] -> Type where
  There :: NS p xs -> NS p (x : xs)
  Here  :: p x     -> NS p (x : xs)

class Family (ki :: kon -> Type) (fam :: [Type]) (codes :: [[[Atom kon]]])
      | fam -> ki codes , ki codes -> fam where
    sfrom' :: SNat ix -> El fam ix -> Rep ki (El fam) (Lkup ix codes)

data Rose a = a :>: [Rose a]
            | Leaf a

type FamRoseInt = '[Rose Int, [Rose Int]]

type CodesRoseInt =
    '[ '[ '[K KInt, I (S Z)], '[K KInt]], '[ '[], '[I Z, I (S Z)]]]

pattern IdxRoseInt = SZ
pattern IdxListRoseInt = SS SZ

pat1 :: PoA Singl (El FamRoseInt) '[I Z, I (S Z)]
     -> NS (PoA Singl (El FamRoseInt)) '[ '[], '[I Z, I (S Z)]]
pat1 d = There (Here d)

pat2 :: PoA Singl (El FamRoseInt) '[]
     -> NS (PoA Singl (El FamRoseInt)) '[ '[], '[I Z, I (S Z)]]
pat2 d = Here d

pat3 :: PoA Singl (El FamRoseInt) '[K KInt]
     -> NS (PoA Singl (El FamRoseInt)) '[ '[K KInt, I (S Z)], '[K KInt]]
pat3 d = There (Here d)

pat4 :: PoA Singl (El FamRoseInt) '[K KInt, I (S Z)]
     -> NS (PoA Singl (El FamRoseInt)) '[ '[K KInt, I (S Z)], '[K KInt]]
pat4 d = Here d

instance Family Singl FamRoseInt CodesRoseInt where
  sfrom' = \case IdxRoseInt     -> \case El (x :>: xs) -> Rep (pat4 (NA_K (SInt x) :* (NA_I (El xs) :* NP0)))
                                         El (Leaf x)   -> Rep (pat3 (NA_K (SInt x) :* NP0))
                 IdxListRoseInt -> \case El []     -> Rep (pat2 NP0)
                                         El (x:xs) -> Rep (pat1 (NA_I (El x) :* (NA_I (El xs) :* NP0)))

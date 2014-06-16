{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module T5329 where

data PZero
data PSucc p

data Peano n where
  PZero ∷ Peano PZero
  PSucc ∷ IsPeano p ⇒ Peano p → Peano (PSucc p)

class IsPeano n where
  peano ∷ Peano n

instance IsPeano PZero where
  peano = PZero

instance IsPeano p ⇒ IsPeano (PSucc p) where
  peano = PSucc peano 

class (n ~ PSucc (PPred n)) ⇒ PHasPred n where
  type PPred n

instance PHasPred (PSucc p) where
  type PPred (PSucc p) = p

pPred ∷ Peano (PSucc p) → Peano p
pPred (PSucc p) = p

infixl 6 :+:

class (IsPeano n, IsPeano m, IsPeano (n :+: m), (n :+: m) ~ (m :+: n))
      ⇒ PAdd n m where
  type n :+: m

instance PAdd PZero PZero where
  type PZero :+: PZero = PZero

instance IsPeano p ⇒ PAdd PZero (PSucc p) where
  type PZero :+: (PSucc p) = PSucc p

instance IsPeano p ⇒ PAdd (PSucc p) PZero where
  type (PSucc p) :+: PZero = PSucc p

instance (IsPeano n, IsPeano m, PAdd n m) ⇒ PAdd (PSucc n) (PSucc m) where
  type (PSucc n) :+: (PSucc m) = PSucc (PSucc (n :+: m))

data PAddResult n m r where
  PAddResult ∷ (PAdd n m, PAdd m n, (n :+: m) ~ r)
             ⇒ PAddResult n m r

pAddLeftZero ∷ ∀ n . IsPeano n ⇒ PAddResult PZero n n
pAddLeftZero = case peano ∷ Peano n of
  PZero   → PAddResult
  PSucc _ → PAddResult

pAddRightZero ∷ ∀ n . IsPeano n ⇒ PAddResult n PZero n
pAddRightZero = case peano ∷ Peano n of
  PZero   → PAddResult
  PSucc _ → PAddResult

data PAddSucc n m where
  PAddSucc ∷ (PAdd n m, PAdd m n,
              PAdd (PSucc n) m, PAdd m (PSucc n),
              PAdd n (PSucc m), PAdd (PSucc m) n,
              (PSucc n :+: m) ~ PSucc (n :+: m),
              (n :+: PSucc m) ~ PSucc (n :+: m))
           ⇒ PAddSucc n m

pAddSucc ∷ ∀ n m . (IsPeano n, IsPeano m) ⇒ PAddSucc n m
pAddSucc = case (peano ∷ Peano n, peano ∷ Peano m) of
  (PZero,   PZero)   → PAddSucc
  (PZero,   PSucc _) → case pAddLeftZero ∷ PAddResult n (PPred m) (PPred m) of
    PAddResult → PAddSucc
  (PSucc _, PZero)   → case pAddRightZero ∷ PAddResult (PPred n) m (PPred n) of
    PAddResult → PAddSucc
  (PSucc _, PSucc _) → case pAddSucc ∷ PAddSucc (PPred n) (PPred m) of
    PAddSucc → PAddSucc

data PAdd2 n m where
  PAdd2 ∷ (PAdd n m, PAdd m n) ⇒ PAdd2 n m

pAdd2 ∷ ∀ n m . (IsPeano n, IsPeano m) ⇒ PAdd2 n m
pAdd2 = case (peano ∷ Peano n, peano ∷ Peano m) of
  (PZero,   PZero)   → PAdd2
  (PZero,   PSucc _) → PAdd2
  (PSucc _, PZero)   → PAdd2
  (PSucc _, PSucc _) → case pAdd2 ∷ PAdd2 (PPred n) (PPred m) of
    PAdd2 → PAdd2

data PAdd3 n m k where
  PAdd3 ∷ (PAdd n m, PAdd m k, PAdd m n, PAdd k m, PAdd n k, PAdd k n,
           PAdd (n :+: m) k, PAdd k (m :+: n),
           PAdd n (m :+: k), PAdd (m :+: k) n,
           PAdd (n :+: k) m, PAdd m (n :+: k),
           ((n :+: m) :+: k) ~ (n :+: (m :+: k)),
           (m :+: (n :+: k)) ~ ((m :+: n) :+: k))
        ⇒ PAdd3 n m k

pAdd3 ∷ ∀ n m k . (IsPeano n, IsPeano m, IsPeano k) ⇒ PAdd3 n m k
pAdd3 = case (peano ∷ Peano n, peano ∷ Peano m, peano ∷ Peano k) of
  (PZero,   PZero,   PZero)   → PAdd3
  (PZero,   PZero,   PSucc _) → PAdd3
  (PZero,   PSucc _, PZero)   → PAdd3
  (PSucc _, PZero,   PZero)   → PAdd3
  (PZero,   PSucc _, PSucc _) →
    case pAdd2 ∷ PAdd2 (PPred m) (PPred k) of
      PAdd2 → PAdd3
  (PSucc _, PZero,   PSucc _) →
    case pAdd2 ∷ PAdd2 (PPred n) (PPred k) of
      PAdd2 → PAdd3
  (PSucc _, PSucc _, PZero)   →
    case pAdd2 ∷ PAdd2 (PPred n) (PPred m) of
      PAdd2 → PAdd3
  (PSucc _, PSucc _, PSucc _) → 
    case pAdd3 ∷ PAdd3 (PPred n) (PPred m) (PPred k) of
      PAdd3 → case pAddSucc ∷ PAddSucc (PPred n :+: PPred m) (PPred k) of
        PAddSucc → case pAddSucc ∷ PAddSucc (PPred n :+: PPred k) (PPred m) of
          PAddSucc → case pAddSucc ∷ PAddSucc (PPred m :+: PPred k) (PPred n) of 
            PAddSucc → PAdd3


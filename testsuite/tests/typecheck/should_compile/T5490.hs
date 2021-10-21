{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Bug (await, bug) where

import Data.Typeable
import Data.Functor
import Control.Exception

data Attempt α = Success α
               | ∀ e . Exception e ⇒ Failure e

fromAttempt ∷ Attempt α → IO α
fromAttempt (Success a) = return a
fromAttempt (Failure e) = throwIO e

data Inject f α = ∀ β . Inject (f β) (α → β)

class Completable f where
  complete ∷ f α → α → IO Bool

instance Completable f ⇒ Completable (Inject f) where
  complete (Inject f inj) = complete f . inj

class Awaitable f where
  awaitResult ∷ f α → IO α

data FakeFuture α = FakeFuture

instance Completable FakeFuture where
  complete _ _ = undefined

instance Awaitable FakeFuture where
  awaitResult _ = undefined

class WaitOp op where
  type WaitOpResult op
  registerWaitOp ∷ Completable f
                 ⇒ op → f (Attempt (WaitOpResult op)) → IO Bool

await ∷ WaitOp op ⇒ op → IO (WaitOpResult op)
await op = do
  let fut = FakeFuture
  registerWaitOp op fut
  fromAttempt =<< awaitResult fut

data FakeOp α = FakeOp

instance WaitOp (FakeOp α) where
  type WaitOpResult (FakeOp α) = α
  registerWaitOp _ _ = return True

data WaitOps rs where
  WaitOp ∷ WaitOp op ⇒ op → WaitOps (HSingle (WaitOpResult op))
  (:?)   ∷ (WaitOp op, HNonEmpty rs)
         ⇒ op → WaitOps rs → WaitOps (WaitOpResult op :* rs)

waitOpsNonEmpty ∷ ∀ rs . WaitOps rs → HNonEmptyInst rs
waitOpsNonEmpty (WaitOp _) = HNonEmptyInst
waitOpsNonEmpty (_ :? _)   = HNonEmptyInst

infixr 7 .?
infix  8 .?.

(.?) ∷ WaitOp op ⇒ op → WaitOps rs → WaitOps (WaitOpResult op :* rs)
op .? ops = case waitOpsNonEmpty ops of
  HNonEmptyInst → op :? ops

(.?.) ∷ (WaitOp op1, WaitOp op2) ⇒ op1 → op2
      → WaitOps (WaitOpResult op1 :*: WaitOpResult op2)
op1 .?. op2 = op1 .? WaitOp op2

data NthException n e = NthException (Peano n) e deriving (Typeable, Show)

instance (Typeable n, Exception e) ⇒ Exception (NthException n e)

instance WaitOp (WaitOps rs) where
  type WaitOpResult (WaitOps rs) = HElemOf rs
  registerWaitOp ops ev = do
    let inj n (Success r) = Success (HNth n r)
        inj n (Failure e) = Failure (NthException n e)
        register ∷ ∀ n . HDropClass n rs
                 ⇒ Bool → Peano n → WaitOps (HDrop n rs) → IO Bool
        register first n (WaitOp op) = do
          t ← try $ registerWaitOp op (Inject ev $ inj n)
          r ← case t of
            Right r → return r
            Left e  → complete ev $ inj n $ Failure (e ∷ SomeExceptionWithLocation)
          return $ r || not first
        register first n (op :? ops') = do
          t ← try $ registerWaitOp op (Inject ev $ inj n)
          case t of
            Right True → case waitOpsNonEmpty ops' of
              HNonEmptyInst → case hTailDropComm ∷ HTailDropComm n rs of
                HTailDropComm → register False (PSucc n) ops'
            Right False → return $ not first
            Left e → do
              c ← complete ev $ inj n $ Failure (e ∷ SomeExceptionWithLocation)
              return $ c || not first
    case waitOpsNonEmpty ops of
      HNonEmptyInst → register True PZero ops

bug ∷ IO Int
bug = do
  temp ← await ((FakeOp ∷ FakeOp Int) .?. (FakeOp ∷ FakeOp String))
  case temp of
    (elem0 → Just _) → return 0
    _                → return 1

data PZero deriving Typeable
data PSucc p deriving Typeable

data Peano n where
  PZero ∷ Peano PZero
  PSucc ∷ IsPeano p ⇒ Peano p → Peano (PSucc p)

instance Show (Peano n) where
  show n = show (peanoNum n ∷ Int)

peanoNum ∷ Num n ⇒ Peano p → n
peanoNum PZero     = 0
peanoNum (PSucc p) = 1 + peanoNum p

class Typeable n ⇒ IsPeano n where
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

infixr 7 :*, .*
infix  8 :*:, .*.

data HNil
data h :* t
type HSingle α = α :* HNil
type α :*: β = α :* β :* HNil

data HList l where
  HNil ∷ HList HNil
  (:*) ∷ HListClass t ⇒ h → HList t → HList (h :* t)

instance Show (HList HNil) where
  show _ = "HNil"

instance (Show h, Show (HList t)) ⇒ Show (HList (h :* t)) where
  showsPrec d (h :* t) = showParen (d > 7) $
    showsPrec 8 h . showString " .* " . showsPrec 7 t

(.*) ∷ HListClass t ⇒ h → HList t → HList (h :* t)
(.*) = (:*)

(.*.) ∷ α → β → HList (α :*: β)
a .*. b = a .* b .* HNil

data HListWitness l where
  HNilList  ∷ HListWitness HNil
  HConsList ∷ HListClass t ⇒ HListWitness (h :* t)

class HListClass l where
  hListWitness ∷ HListWitness l

instance HListClass HNil where
  hListWitness = HNilList

instance HListClass t ⇒ HListClass (h :* t) where
  hListWitness = HConsList

data HListInst l where
  HListInst ∷ HListClass l ⇒ HListInst l

hListInst ∷ HList l → HListInst l
hListInst HNil     = HListInst
hListInst (_ :* _) = HListInst

class (l ~ (HHead l :* HTail l), HListClass (HTail l)) ⇒ HNonEmpty l where
  type HHead l
  type HTail l

instance HListClass t ⇒ HNonEmpty (h :* t) where
  type HHead (h :* t) = h
  type HTail (h :* t) = t

hHead ∷ HList (h :* t) → h
hHead (h :* _) = h

hTail ∷ HList (h :* t) → HList t
hTail (_ :* t) = t

data HNonEmptyInst l where
  HNonEmptyInst ∷ HListClass t ⇒ HNonEmptyInst (h :* t)

data HDropWitness n l where
  HDropZero ∷ HListClass l ⇒ HDropWitness PZero l
  HDropSucc ∷ HDropClass p t ⇒ HDropWitness (PSucc p) (h :* t)

class (IsPeano n, HListClass l, HListClass (HDrop n l)) ⇒ HDropClass n l where
  type HDrop n l
  hDropWitness ∷ HDropWitness n l

instance HListClass l ⇒ HDropClass PZero l where
  type HDrop PZero l = l
  hDropWitness = HDropZero

instance HDropClass p t ⇒ HDropClass (PSucc p) (h :* t) where
  type HDrop (PSucc p) (h :* t) = HDrop p t
  hDropWitness = case hDropWitness ∷ HDropWitness p t of
    HDropZero → HDropSucc
    HDropSucc → HDropSucc

data HDropInst n l where
  HDropInst ∷ HDropClass n l ⇒ HDropInst n l

hDrop ∷ ∀ n l . HDropClass n l ⇒ Peano n → HList l → HList (HDrop n l)
hDrop n l = case hDropWitness ∷ HDropWitness n l of
  HDropZero → l
  HDropSucc → hDrop (pPred n) (hTail l)

data HNonEmptyDropInst n l where
  HNonEmptyDropInst ∷ (HDropClass n l, HNonEmpty l,
                       HDropClass (PSucc n) l, HNonEmpty (HDrop n l))
                    ⇒ HNonEmptyDropInst n l

pPrevDropInst ∷ ∀ n l . HDropClass (PSucc n) l ⇒ HNonEmptyDropInst n l
pPrevDropInst = case hDropWitness ∷ HDropWitness (PSucc n) l of
  HDropSucc → case hDropWitness ∷ HDropWitness n (HTail l) of
    HDropZero → HNonEmptyDropInst
    HDropSucc → case pPrevDropInst ∷ HNonEmptyDropInst (PPred n) (HTail l) of
      HNonEmptyDropInst → HNonEmptyDropInst

hNextDropInst ∷ ∀ n l . (HDropClass n l, HNonEmpty (HDrop n l))
              ⇒ HNonEmptyDropInst n l
hNextDropInst = case hDropWitness ∷ HDropWitness n l of
  HDropZero → HNonEmptyDropInst
  HDropSucc → case hNextDropInst ∷ HNonEmptyDropInst (PPred n) (HTail l) of
    HNonEmptyDropInst → HNonEmptyDropInst

data HTailDropComm n l where
  HTailDropComm ∷ (HNonEmpty l, HDropClass n l,
                   HNonEmpty (HDrop n l), HDropClass n (HTail l),
                   HDropClass (PSucc n) l,
                   HTail (HDrop n l) ~ HDrop n (HTail l),
                   HDrop (PSucc n) l ~ HTail (HDrop n l),
                   HDrop (PSucc n) l ~ HDrop n (HTail l))
                ⇒ HTailDropComm n l

hTailDropComm' ∷ ∀ n l . (HDropClass (PSucc n) l)
               ⇒ HTailDropComm n l
hTailDropComm' = case pPrevDropInst ∷ HNonEmptyDropInst n l of
  HNonEmptyDropInst → hTailDropComm

hTailDropComm ∷ ∀ n l . (HDropClass n l, HNonEmpty (HDrop n l))
               ⇒ HTailDropComm n l
hTailDropComm = case hDropWitness ∷ HDropWitness n l of
  HDropZero → HTailDropComm
  HDropSucc  → case hTailDropComm ∷ HTailDropComm (PPred n) (HTail l) of
    HTailDropComm → HTailDropComm

type HNth n l = HHead (HDrop n l)

data HElemOf l where
  HNth ∷ (HDropClass n l, HNonEmpty (HDrop n l))
       ⇒ Peano n → HNth n l → HElemOf l

hGetIfNth ∷ ∀ n l . (HDropClass n l, HNonEmpty (HDrop n l))
          ⇒ Peano n → HElemOf l → Maybe (HNth n l)
hGetIfNth PZero    (HNth PZero x)     = Just x
hGetIfNth (PSucc p) (HNth (PSucc p') x) =
  case hDropWitness ∷ HDropWitness n l of
    HDropSucc  →
      let inst ∷ ∀ m . HDropClass (PSucc m) l
               ⇒ Peano m → HTailDropComm m l
          inst _ = hTailDropComm' in
        case inst p' of
          HTailDropComm → hGetIfNth p (HNth p' x ∷ HElemOf (HTail l))
    _          → undefined
hGetIfNth _        _                  = Nothing

elem0 ∷ HNonEmpty l ⇒ HElemOf l → Maybe (HHead l)
elem0 = hGetIfNth PZero

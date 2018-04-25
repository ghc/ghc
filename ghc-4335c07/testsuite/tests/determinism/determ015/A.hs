{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module A where

infixr 7 :*
infix  8 :*:

data HNil
data α :* β
type HSingle α = α :* HNil
type α :*: β = α :* β :* HNil

data HList l where
  HNil  ∷ HList HNil
  (:*) ∷ α → HList t → HList (α :* t)

data First
data Next p

data HIndex i where
  First ∷ HIndex First
  Next  ∷ HIndex p → HIndex (Next p)

class (l ~ (HHead l :* HTail l)) ⇒ HNonEmpty l where
  type HHead l
  type HTail l

instance HNonEmpty (h :* t) where
  type HHead (h :* t) = h
  type HTail (h :* t) = t

data HFromWitness n l where
  HFromFirst ∷ HFromWitness First l
  HFromNext  ∷ (HNonEmpty l, HFromClass p (HTail l),
                HTail (HFrom (Next p) l) ~ HFrom (Next p) (HTail l))
             ⇒ HFromWitness (Next p) l

class HFromClass n l where
  type HFrom n l
  hFromWitness ∷ HFromWitness n l

instance HFromClass First l where
  type HFrom First l = l
  hFromWitness = HFromFirst

instance (HNonEmpty l, HFromClass p (HTail l)) ⇒ HFromClass (Next p) l where
  type HFrom (Next p) l = HFrom p (HTail l)
  hFromWitness = case hFromWitness ∷ HFromWitness p (HTail l) of
    HFromFirst → HFromNext
    HFromNext  → HFromNext

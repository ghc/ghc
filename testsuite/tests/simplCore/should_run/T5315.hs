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

infixr 7 :*, .*
infix  8 :*:, .*.

data HNil
data α :* β
type HSingle α = α :* HNil
type α :*: β = α :* β :* HNil

data HList l where
  HNil  ∷ HList HNil
  (:*) ∷ α → HList t → HList (α :* t)

(.*) ∷ α → HList t → HList (α :* t)
(.*) = (:*)

(.*.) ∷ α → β → HList (α :*: β)
a .*. b = a .* b .* HNil

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

hHead ∷ HNonEmpty l ⇒ HList l → HHead l
hHead (h :* _) = h
hHead _        = undefined

hTail ∷ HNonEmpty l ⇒ HList l → HList (HTail l)
hTail (_ :* t) = t
hTail _        = undefined

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

hFrom ∷ ∀ n l . HFromClass n l ⇒ HIndex n → HList l → HList (HFrom n l)
hFrom First    l = l
hFrom (Next p) l = case hFromWitness ∷ HFromWitness n l of
  HFromNext  → hFrom p (hTail l)
  _          → undefined

type HNth n l = HHead (HFrom n l)

hNth ∷ ∀ n l . (HFromClass n l, HNonEmpty (HFrom n l))
     ⇒ HIndex n → HList l → HNth n l
hNth First l    = hHead l
hNth (Next p) l = case hFromWitness ∷ HFromWitness n l of
  HFromNext → hNth p (hTail l)
  _         → undefined

main = putStrLn $ hNth (Next First) (0 .*. "Test")


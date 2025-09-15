{-# LANGUAGE DataKinds, PolyKinds, UnicodeSyntax, GADTs, NoImplicitPrelude,
             TypeOperators, TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

module TypeLevelVec where

import Data.Kind

data ℕ ∷ Type where
  O ∷ ℕ
  S ∷ ℕ → ℕ

type family x + y where
  O   + n = n
  S m + n = S (m + n)
infixl 5 +

data Vec ∷ ℕ → Type → Type where
  Nil  ∷ Vec O a
  (:>) ∷ a → Vec n a → Vec (S n) a
infixr 8 :>

type (++) ∷ Vec n a → Vec m a → Vec (n + m) a
type family x ++ y where
  Nil       ++ y = y
  (x :> xs) ++ y = x :> (xs ++ y)
infixl 5 ++

{-# LANGUAGE UnicodeSyntax, MagicHash, TypeInType, TypeFamilies #-}

-- from Conal Elliott
-- Actually, this *should* work. But I want to put it in the testsuite
-- as a succeeding "compile_fail" test to make sure that we don't panic.

module RepRep where

import GHC.Exts

type family RepRep a ∷ RuntimeRep

class HasRep a where
  type Rep a ∷ TYPE (RepRep a)
  repr ∷ a → Rep a
  abst ∷ Rep a → a

type instance RepRep Int = IntRep

instance HasRep Int where
  type Rep Int = Int#
  abst n = I# n
  repr (I# n) = n

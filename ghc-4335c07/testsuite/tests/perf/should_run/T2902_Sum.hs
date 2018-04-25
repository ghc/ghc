
{-# LANGUAGE UnicodeSyntax, MultiParamTypeClasses #-}

module T2902_Sum (Sum(..)) where

class Sum c a b where
  insert     ∷ a → b → c a b → c a b
  union      ∷ c a b → c a b → c a b
  unions     ∷ [c a b] → c a b
  extractMin ∷ c a b → ((a,b), c a b)

  fromList   ∷ [(a,b)] → c a b
  toList     ∷ c a b → [(a,b)]


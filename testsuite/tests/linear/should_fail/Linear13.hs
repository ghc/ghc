{-# LANGUAGE UnicodeSyntax #-}
module Linear13 where

incorrectLet :: a ⊸ ()
incorrectLet a = let x = a in ()

incorrectLazyMatch :: (a,b) ⊸ b
incorrectLazyMatch x = let (a,b) = x in b

incorrectCasePromotion :: (a,b) ⊸ b
incorrectCasePromotion x = case x of (a,b) -> b

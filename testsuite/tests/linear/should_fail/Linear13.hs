{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE UnicodeSyntax #-}
module Linear13 where

incorrectLet :: a ⊸ ()
incorrectLet a = let x = a in ()

incorrectLetWithSignature :: (Bool->Bool) %1 -> ()
incorrectLetWithSignature x = let y :: Bool->Bool; y = x in ()

incorrectLazyMatch :: (a,b) ⊸ b
incorrectLazyMatch x = let (a,b) = x in b

incorrectCasePromotion :: (a,b) ⊸ b
incorrectCasePromotion x = case x of (a,b) -> b

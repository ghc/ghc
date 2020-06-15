{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE UnicodeSyntax #-}
module Linear5 where

incorrectEqn :: Bool -> Int ⊸ Int
incorrectEqn True  n = n
incorrectEqn False n = 0

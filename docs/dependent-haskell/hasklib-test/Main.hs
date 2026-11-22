module Main where

import Client (checkAppendSingleton, zipLaw)
import HaskLib

-- | Use the installed list proofs from an ordinary Haskell function.
check :: (Bool -> Bool) -> (Bool -> Bool -> Bool) -> [Bool] -> [Bool] -> Bool
check pre f xs ys = case lengthMap pre xs of
    Refl -> case lengthAppend xs ys of
        Refl -> case zipLaw xs ys of
            Refl -> case lengthZipWith f xs ys of Refl -> True

main :: IO ()
main = do
    let bad = error "forced zipWith callback"
        element = error "forced an append element"
    if check not bad [True, False] [False]
        && check not bad [] (error "forced right list")
        && checkAppendSingleton [] element
        && checkAppendSingleton [element, element] element
        then pure ()
        else fail "list proof failed"

module Main where

import Client (checkAppendSingleton, checkReverse, checkZipMap, zipLaw)
import HaskLib

-- | Use the installed list proofs from an ordinary Haskell function.
check :: (Bool -> Bool) -> (Bool -> Bool -> Bool) -> [Bool] -> [Bool] -> Bool
check pre f xs ys = case lengthMap pre xs of
    Refl -> case lengthAppend xs ys of
        Refl -> case zipLaw xs ys of
            Refl -> case lengthZipWith f xs ys of Refl -> True

-- | Consume a prefix-length witness without inspecting the elements.
checkPrefix :: Size -> [Bool] -> Bool
checkPrefix n xs = case lengthTakeSize n xs of Refl -> True

main :: IO ()
main = do
    let bad = error "forced zipWith callback"
        element = error "forced an append element"
        tailList = error "forced the list beyond the bound"
        tailBound = error "forced the bound beyond the list"
    if check not bad [True, False] [False]
        && check not bad [] (error "forced right list")
        && checkAppendSingleton [] element
        && checkAppendSingleton [element, element] element
        && checkReverse []
        && checkReverse [element, element]
        && checkZipMap (error "forced map callback") (error "forced zip callback") [element] [0]
        && checkPrefix Zero tailList
        && checkPrefix (Succ tailBound) []
        && checkPrefix (Succ Zero) (element : tailList)
        && checkPrefix (Succ (Succ Zero)) (repeat element)
        && checkPrefix (Succ (Succ Zero)) [element]
        && takeSize (Succ Zero) (True : tailList) == [True]
        then pure ()
        else fail "list proof failed"

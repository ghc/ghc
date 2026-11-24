module Main where

import Control.Exception (ErrorCall(..), evaluate, try)
import Client (addUnder, checkAppendSingleton, checkNat, checkReverse, checkZipMap, zipLaw)
import HaskLib

-- | Use the installed list proofs from an ordinary Haskell function.
check :: (Bool -> Bool) -> (Bool -> Bool -> Bool) -> [Bool] -> [Bool] -> Bool
check pre f xs ys = case lengthMap pre xs of
    Refl -> case lengthAppend xs ys of
        Refl -> case zipLaw xs ys of
            Refl -> case lengthZipWith f xs ys of Refl -> True

-- | Consume a prefix-length witness without inspecting the elements.
checkPrefix :: Nat -> [Bool] -> Bool
checkPrefix n xs = case lengthTakeSize n xs of Refl -> True

main :: IO ()
main = do
    let undefinedBound = error "forced the prefix bound"
    strictBound <- try (evaluate (takeSize undefinedBound [] :: [Bool]))
    let boundWasForced = case strictBound of
            Left (ErrorCall "forced the prefix bound") -> True
            _ -> False
        bad = error "forced zipWith callback"
        element = error "forced an append element"
        tailList = error "forced the list beyond the bound"
        tailBound = error "forced the bound beyond the list"
    if check not bad [True, False] [False]
        && check not bad [] (error "forced right list")
        && (case addUnder Zero (Succ Zero) Zero of Refl -> True)
        && checkAppendSingleton [] element
        && checkAppendSingleton [element, element] element
        && checkReverse []
        && checkReverse [element, element]
        && checkZipMap (error "forced map callback") (error "forced zip callback") [element] [0]
        && checkNat
        && boundWasForced
        && checkPrefix Zero tailList
        && checkPrefix (Succ tailBound) []
        && null (takeSize (Succ tailBound) ([] :: [Bool]))
        && checkPrefix (Succ Zero) (element : tailList)
        && checkPrefix (Succ (Succ Zero)) (repeat element)
        && checkPrefix (Succ (Succ Zero)) [element]
        && takeSize (Succ Zero) (True : tailList) == [True]
        then pure ()
        else fail "list proof failed"

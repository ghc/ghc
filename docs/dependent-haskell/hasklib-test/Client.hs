{-# LANGUAGE ExperimentalDependentTypes #-}
{-# OPTIONS_GHC -fno-enable-rewrite-rules #-}

module Client (zipLaw, lengthAppendSingleton, checkAppendSingleton, checkReverse, checkZipMap) where

import qualified HaskLib as H

-- | Use the public functions and equality in a new dependent signature.
zipLaw :: forall a b. foreach (xs :: [a]) -> foreach (ys :: [b])
       -> H.size (zip xs ys) H.:~: H.minSize (H.size xs) (H.size ys)
zipLaw xs ys = H.lengthZip xs ys

-- | Appending one element adds one list cell.
lengthAppendSingleton :: forall a. foreach (xs :: [a]) -> foreach (x :: a)
                      -> H.size (xs ++ '[x]) H.:~: 'H.Succ (H.size xs)
lengthAppendSingleton xs x =
    H.lengthAppend xs [x]
        `H.trans` H.plusSucc (H.size xs) H.Zero
        `H.trans` H.congSucc (H.plusZero (H.size xs))

-- | Check the proof without inspecting any list elements.
checkAppendSingleton :: [Bool] -> Bool -> Bool
checkAppendSingleton xs x = case lengthAppendSingleton xs x of
    H.Refl -> True

-- | Check a reverse proof from an ordinary caller.
checkReverse :: [Bool] -> Bool
checkReverse xs = case H.lengthReverse xs of
    H.Refl -> True

-- | Compose two library proofs. The map proof visits the entire left spine.
zipMapLaw :: forall a b c d. foreach (pre :: a -> b) -> foreach (combine :: b -> c -> d)
           -> foreach (xs :: [a]) -> foreach (ys :: [c])
           -> H.size (zipWith combine (map pre xs) ys) H.:~: H.minSize (H.size xs) (H.size ys)
zipMapLaw pre combine xs ys =
    H.lengthZipWith combine (map pre xs) ys
        `H.trans` H.congFirst @H.minSize @(H.size ys) (H.lengthMap pre xs)

-- | Instantiate the pipeline with different intermediate and input types.
checkZipMap :: (Bool -> Char) -> (Char -> Int -> Ordering) -> [Bool] -> [Int] -> Bool
checkZipMap pre combine xs ys =
    case zipMapLaw pre combine xs ys of H.Refl -> True

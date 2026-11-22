{-# LANGUAGE ExperimentalDependentTypes #-}
{-# OPTIONS_GHC -fno-enable-rewrite-rules #-}

module Client (zipLaw, lengthAppendSingleton, checkAppendSingleton) where

import qualified HaskLib as H
import HaskLib.Size (plusSucc, plusZero)

-- | Use the public functions and equality in a new dependent signature.
zipLaw :: forall a b. foreach (xs :: [a]) -> foreach (ys :: [b])
       -> H.size (zip xs ys) H.:~: H.minSize (H.size xs) (H.size ys)
zipLaw xs ys = H.lengthZip xs ys

-- | Appending one element adds one list cell.
lengthAppendSingleton :: forall a. foreach (xs :: [a]) -> foreach (x :: a)
                      -> H.size (xs ++ '[x]) H.:~: 'H.Succ (H.size xs)
lengthAppendSingleton xs x =
    case H.lengthAppend xs [x] of
        H.Refl -> case plusSucc (H.size xs) H.Zero of
            H.Refl -> case plusZero (H.size xs) of
                H.Refl -> H.Refl

-- | Check the proof without inspecting any list elements.
checkAppendSingleton :: [Bool] -> Bool -> Bool
checkAppendSingleton xs x = case lengthAppendSingleton xs x of
    H.Refl -> True

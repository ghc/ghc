{-# LANGUAGE ExperimentalDependentTypes #-}
{-# OPTIONS_GHC -fno-enable-rewrite-rules #-}
module PreludeDropWhileRuntime (showPrefixAndSuffix) where

import PreludeDropWhileProof (prefixAndSuffix)
import PreludeFilterAppendRuntime (decisionsForNot)

-- | Show the proof and the first two elements of the reconstructed and original lists.
showPrefixAndSuffix :: foreach (xs :: [Bool]) -> String
showPrefixAndSuffix xs =
    show (prefixAndSuffix (decisionsForNot xs), take 2 reconstructed, take 2 xs)
  where
    reconstructed = takeWhile not xs ++ dropWhile not xs
